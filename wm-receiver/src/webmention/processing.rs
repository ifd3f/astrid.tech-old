use std::{error::Error, ops::Add, path::Path};

use chrono::{DateTime, Duration, NaiveDateTime, TimeZone, Utc};
use scraper::{Html, Selector};
use url::Url;

use crate::{
    db::get_db,
    schema::requests,
    webmention::storage::{read_existing_webmention, StorageAction},
};

use super::data::{RelUrl, WebmentionRecord};

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

struct GatheredWebmentionData {
    /// The pending request data we stored in the database.
    request: PendingRequest,
    /// The rel_url data we found for this mention.
    rel_url: Option<RelUrl>,
}

impl PendingRequest {
    pub async fn process(self, wm_dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
        let db = get_db();

        let html = self.get_html().await?;
        let rel_url = self.extract_data_from_html(html.as_str());
        let existing_mention = read_existing_webmention(
            &wm_dir,
            Url::parse(self.source_url.as_str()).unwrap(),
            Url::parse(self.target_url.as_str()).unwrap(),
        );
        let now = Utc::now();

        let (new_mention, processed) = GatheredWebmentionData {
            request: self,
            rel_url,
        }
        .parse_to_mention(now);

        let action = match (existing_mention, new_mention) {
            // Invalid webmention
            (None, None) => None,

            // Create
            (None, Some(n)) => Some(StorageAction::Write(n)),

            // Delete
            (Some(e), None) => Some(StorageAction::Delete {
                source_url: e.source_url,
                target_url: e.target_url,
            }),
            
            // Update
            (Some(e), Some(n)) => {
                // Only update if the context of the mention got updated
                if e.rel_url != n.rel_url {
                    Some(StorageAction::Write(n))
                } else {
                    None
                }
            }
        };

        if let Some(action) = action {
            action.apply(wm_dir)?;
        }

        {
            use crate::schema::requests::dsl::*;
            use diesel::prelude::*;

            diesel::update(requests).set(&processed).execute(&db)?;
        }

        Ok(())
    }

    async fn get_html(&self) -> Result<String, Box<dyn std::error::Error>> {
        let resp = reqwest::get(self.source_url.as_str()).await?.text().await?;
        Ok(resp)
    }

    fn extract_data_from_html(&self, html: &'h str) -> Option<RelUrl> {
        let anchor_selector: Selector = Selector::parse("a").unwrap();

        for element in Html::parse_fragment(html).select(&anchor_selector) {
            let text = element.text().into_iter().collect::<String>();
            let href = if let Some(href) = element.value().attr("href") {
                href
            } else {
                continue;
            };

            let rel = element.value().attr("rel");

            if href == self.target_url {
                let rels: Vec<String> = rel
                    .map(|r| r.to_string())
                    .map_or_else(Vec::new, |x| vec![x]);
                return Some(RelUrl { rels, text });
            }
        }
        None
    }
}

impl GatheredWebmentionData {
    fn parse_to_mention(self, now: DateTime<Utc>) -> (Option<WebmentionRecord>, ProcessedRequest) {
        let now_naive = now.naive_utc();

        let microformats = if let Some(microformat) = self.rel_url {
            microformat
        } else {
            return (
                None,
                ProcessedRequest {
                    id: self.request.id,
                    processing_status: 1,
                    processing_attempts: self.request.processing_attempts + 1,
                    processed_on: now_naive,
                    next_processing_attempt: Some(now_naive.add(Duration::hours(1))),
                },
            );
        };

        let mentioned_on = Utc.from_utc_datetime(&self.request.mentioned_on);
        let webmention = WebmentionRecord {
            source_url: self.request.source_url.to_string(),
            target_url: self.request.target_url.to_string(),
            mentioned_on,
            processed_on: now,
            rel_url: microformats,
        };
        let processing_status = ProcessedRequest {
            id: self.request.id,
            processing_status: 2,
            processed_on: now_naive,
            next_processing_attempt: None,
            processing_attempts: self.request.processing_attempts + 1,
        };
        (Some(webmention), processing_status)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use chrono::Utc;

    use super::PendingRequest;

    #[test]
    fn extract_from_html_with_valid_target() {
        let request = PendingRequest {
            id: 0,
            source_url: "https://source.com".to_string(),
            target_url: "https://target.com".to_string(),
            processing_attempts: 0,
            mentioned_on: Utc::now().naive_utc(),
        };
        let html = r#"
            <a rel="in-reply-to" href="https://target.com">text</a>
        "#;

        let extracted = request.extract_data_from_html(html).unwrap();

        assert_eq!(extracted.text, "text");
        assert_eq!(extracted.rels, vec!["in-reply-to"]);
    }

    #[test]
    fn extract_from_html_with_different_target() {
        let request = PendingRequest {
            id: 0,
            source_url: "https://source.com".to_string(),
            target_url: "https://target.com/the/given/page".to_string(),
            processing_attempts: 0,
            mentioned_on: Utc::now().naive_utc(),
        };
        let html = r#"
            <a rel="in-reply-to" href="https://target.com/another/page">lol</a>
        "#;

        let extracted = request.extract_data_from_html(html);

        assert_matches!(extracted, None);
    }
}
