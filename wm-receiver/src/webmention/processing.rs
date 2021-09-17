use std::{error::Error, ops::Add};

use chrono::{DateTime, Duration, NaiveDateTime, TimeZone, Utc};
use scraper::{Html, Selector};

use crate::{schema::mentions, webmention::storage::StorageAction};

use super::data::{RelUrl, Webmention};

#[derive(Queryable, Identifiable, Debug)]
#[table_name = "mentions"]
struct PendingRequest<'a> {
    id: i32,
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_attempts: i32,
    mentioned_on: NaiveDateTime,
    processed_on: Option<NaiveDateTime>,
}

#[derive(Identifiable, Debug)]
#[table_name = "mentions"]
struct ProcessedRequest {
    id: i32,
    processing_status: i32,
    processing_attempts: i32,
    processed_on: NaiveDateTime,
    next_processing_attempt: Option<NaiveDateTime>,
}

struct GatheredWebmentionData<'a> {
    /// The pending request data we stored in the database.
    request: PendingRequest<'a>,
    /// The rel_url data we found for this mention.
    rel_url: Option<RelUrl>,
    /// The existing webmention data.
    existing: Option<Webmention<'a>>,
}

impl<'a> PendingRequest<'a> {
    pub async fn process(self) -> Result<(), Box<dyn Error>>{
        let html = self.get_html().await?;
        let rel_url = self.extract_data_from_html(html.as_str());
        let existing_mention = todo!() as Option<Webmention>;
        let now = Utc::now();

        let (new_mention, result) = GatheredWebmentionData {
            request: self,
            rel_url,
            existing: existing_mention,
        }
        .parse_to_mention(now);

        let action = match (existing_mention, new_mention) {
            (None, None) => todo!(),
            (None, Some(n)) => StorageAction::CreateWebmention(n),
            (Some(e), None) => StorageAction::DeleteWebmention {source_url: e.source_url, target_url: e.target_url },
            (Some(e), Some(n)) => StorageAction::UpdateWebmention(n)
        };
    }

    async fn get_html(&self) -> Result<String, Box<dyn std::error::Error>> {
        let resp = reqwest::get(self.source_url).await?.text().await?;
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

impl<'a> GatheredWebmentionData<'a> {
    fn parse_to_mention(self, now: DateTime<Utc>) -> (Option<Webmention<'a>>, ProcessedRequest) {
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
        let webmention = Webmention {
            source_url: self.request.source_url,
            target_url: self.request.target_url,
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
            source_url: "https://source.com",
            target_url: "https://target.com",
            sender_ip: "1.2.3.4",
            processing_attempts: 0,
            mentioned_on: Utc::now().naive_utc(),
            processed_on: None,
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
            source_url: "https://source.com",
            target_url: "https://target.com/the/given/page",
            sender_ip: "1.2.3.4",
            processing_attempts: 0,
            mentioned_on: Utc::now().naive_utc(),
            processed_on: None,
        };
        let html = r#"
            <a rel="in-reply-to" href="https://target.com/another/page">lol</a>
        "#;

        let extracted = request.extract_data_from_html(html);

        assert_matches!(extracted, None);
    }
}
