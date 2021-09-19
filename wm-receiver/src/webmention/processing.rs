use std::{error::Error, ops::Add};

use chrono::{DateTime, Duration, NaiveDateTime, TimeZone, Utc};
use scraper::{Html, Selector};
use url::Url;

use crate::{db::get_db, schema::requests, webmention::storage::StorageAction};

use super::{
    data::{RelUrl, WebmentionRecord},
    storage::WebmentionStore,
};

#[derive(Queryable, Identifiable, Debug)]
#[table_name = "requests"]
pub struct PendingRequest {
    id: i32,
    source_url: String,
    target_url: String,
    processing_attempts: i32,
    mentioned_on: NaiveDateTime,
}

#[derive(AsChangeset, Identifiable, Debug)]
#[table_name = "requests"]
struct ProcessedRequest {
    id: i32,
    processing_status: i32,
    processing_attempts: i32,
    processed_on: NaiveDateTime,
    next_processing_attempt: Option<NaiveDateTime>,
}

pub async fn process_pending_request(
    pending: PendingRequest,
    wm_store: &mut WebmentionStore,
) -> Result<(), Box<dyn Error>> {
    let db = get_db();

    let wm_store = wm_store;

    let html = pending.fetch_html().await?;
    let rel_url = extract_rel_data_from_html(&pending.target_url, html);
    let existing_mention = wm_store.get_webmention(
        Url::parse(&pending.source_url).unwrap(),
        Url::parse(&pending.target_url).unwrap(),
    );

    let now = Utc::now();
    let (new_mention, processed) = pending.with_verification_result(rel_url, now);
    let action = determine_storage_action(existing_mention, new_mention);

    if let Some(action) = action {
        wm_store.apply(action)?;
    }

    {
        use crate::schema::requests::dsl::*;
        use diesel::prelude::*;

        diesel::update(requests).set(&processed).execute(&db)?;
    }

    Ok(())
}

fn determine_storage_action(
    existing_mention: Option<WebmentionRecord>,
    new_mention: Option<WebmentionRecord>,
) -> Option<StorageAction> {
    match (existing_mention, new_mention) {
        // Webmention failed validation
        (None, None) => None,

        // Create
        (None, Some(n)) => Some(StorageAction::Write(n)),

        // Delete
        (Some(e), None) => Some(StorageAction::Delete {
            source_url: e.source,
            target_url: e.target,
        }),

        // Update
        (Some(e), Some(n)) => {
            // Only update if the rel-url context of the mention got updated
            if e.rel_url != n.rel_url {
                Some(StorageAction::Write(n))
            } else {
                None
            }
        }
    }
}

fn extract_rel_data_from_html(
    target_url: impl AsRef<str>,
    html: impl AsRef<str>,
) -> Option<RelUrl> {
    let anchor_selector: Selector = Selector::parse("a").unwrap();

    for element in Html::parse_fragment(html.as_ref()).select(&anchor_selector) {
        let text = element.text().into_iter().collect::<String>();
        let href = if let Some(href) = element.value().attr("href") {
            href
        } else {
            continue;
        };

        let rel = element.value().attr("rel");

        if href == target_url.as_ref() {
            let rels: Vec<String> = rel
                .map(|r| r.to_string())
                .map_or_else(Vec::new, |x| vec![x]);
            return Some(RelUrl { rels, text });
        }
    }
    None
}

impl PendingRequest {
    async fn fetch_html(&self) -> Result<String, Box<dyn std::error::Error>> {
        let resp = reqwest::get(self.source_url.as_str()).await?.text().await?;
        Ok(resp)
    }

    fn with_verification_result(
        self,
        rel_url: Option<RelUrl>,
        now: DateTime<Utc>,
    ) -> (Option<WebmentionRecord>, ProcessedRequest) {
        let now_naive = now.naive_utc();

        let microformats = if let Some(microformat) = rel_url {
            microformat
        } else {
            // No rel-url associated with the link, meaning this is an invalid webmention
            return (
                None,
                ProcessedRequest {
                    id: self.id,
                    processing_status: 1,
                    processing_attempts: self.processing_attempts + 1,
                    processed_on: now_naive,
                    next_processing_attempt: Some(now_naive.add(Duration::hours(1))),
                },
            );
        };

        let mentioned_on = Utc.from_utc_datetime(&self.mentioned_on);
        let webmention = WebmentionRecord {
            source: self.source_url,
            target: self.target_url,
            mentioned_on,
            processed_on: now,
            rel_url: microformats,
        };
        let processing_status = ProcessedRequest {
            id: self.id,
            processing_status: 2,
            processed_on: now_naive,
            next_processing_attempt: None,
            processing_attempts: self.processing_attempts + 1,
        };
        (Some(webmention), processing_status)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::webmention::processing::extract_rel_data_from_html;

    #[test]
    fn extract_from_html_with_valid_target() {
        let provided_target = "https://target.com".to_string();
        let html = r#"
            <a rel="in-reply-to" href="https://target.com">text</a>
        "#;

        let extracted = extract_rel_data_from_html(provided_target, html).unwrap();

        assert_eq!(extracted.text, "text");
        assert_eq!(extracted.rels, vec!["in-reply-to"]);
    }

    #[test]
    fn extract_from_html_with_different_target() {
        let provided_target = "https://target.com/the/given/page".to_string();
        let html = r#"
            <a rel="in-reply-to" href="https://target.com/another/page">lol</a>
        "#;

        let extracted = extract_rel_data_from_html(provided_target, html);

        assert_matches!(extracted, None);
    }
}
