use chrono::NaiveDateTime;

use crate::schema::mentions;

use super::data::{RelUrl, Webmention};

#[derive(Queryable, Debug)]
struct PendingProcessingMention<'a> {
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_status: i32,
    mentioned_on: NaiveDateTime,
    processed_on: Option<NaiveDateTime>,
}

#[derive(Insertable, Debug)]
#[table_name = "mentions"]
struct ProcessedMention<'a> {
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_status: i32,
    mentioned_on: NaiveDateTime,
    processed_on: Option<NaiveDateTime>,
    next_processing_attempt: Option<NaiveDateTime>,
}

struct ParsingState<'a> {
    mention: PendingProcessingMention<'a>,
    rel_url: RelUrl<'a>,
    existing: Option<Webmention<'a>>,
}
