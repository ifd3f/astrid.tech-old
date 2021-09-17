use std::ops::Add;

use chrono::{DateTime, Duration, NaiveDateTime, Utc};

use crate::schema::mentions;

use super::data::{RelUrl, Webmention};

#[derive(Queryable, Identifiable, Debug)]
#[table_name = "mentions"]
struct PendingRequest<'a> {
    id: i32,
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_status: i32,
    processing_attempts: i32,
    mentioned_on: NaiveDateTime,
    processed_on: Option<NaiveDateTime>,
}

#[derive(Identifiable, Debug)]
#[table_name = "mentions"]
struct ProcessedRequest<'a> {
    id: i32,
    processing_status: i32,
    processing_attempts: i32,
    processed_on: Option<NaiveDateTime>,
    next_processing_attempt: Option<NaiveDateTime>,
}

struct GatheredWebmentionData<'a> {
    /// The pending request data we stored in the database.
    request: PendingRequest<'a>,
    /// The rel_url data we found for this mention.
    rel_url: Option<RelUrl<'a>>,
    /// The existing webmention data.
    existing: Option<Webmention<'a>>,
}

impl<'a> GatheredWebmentionData<'a> {
    fn parse_to_mention(
        self,
        now: DateTime<Utc>,
    ) -> (Option<Webmention<'a>>, ProcessedRequest<'a>) {
        let microformats = if let Some(microformat) = self.rel_url {
            microformats
        } else {
            return (
                None,
                ProcessedRequest {
                    id: self.request.id,
                    processing_status: 1,
                    processing_attempts: self.request.processing_attempts + 1,
                    processed_on: Some(now),
                    next_processing_attempt: Some(now.add(Duration::hours(1))),
                },
            );
        };

        let webmention = Webmention {
            source_url: self.request.source_url,
            target_url: self.request.target_url,
            mentioned_on: self.request.mentioned_on,
            processed_on: now,
            rel_url: microformats,
        };
        let processing_status = ProcessedRequest {
            id: self.request.id,
            processing_status: 2,
            processed_on: Some(now),
            next_processing_attempt: None,
            processing_attempts: self.request.processing_attempts + 1,
        };
        (Some(webmention), processing_status)
    }
}
