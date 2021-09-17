use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct RelUrl<'a> {
    rels: Vec<&'a str>,
    text: &'a str,
}

pub enum MentionProcessingStatus {
    Unprocessed,
    Failed,
    Succeeded,
}

/// A user-friendly processed webmention.
#[derive(Serialize, Deserialize)]
pub struct Webmention<'a> {
    /// The normalized URL of the source that sent the webmention.
    source_url: &'a str,
    /// The normalized URL of the target that is mentioned.
    target_url: &'a str,
    /// When this mention was sent.
    mentioned_on: DateTime<Utc>,
    /// When this mention was processed.
    processed_on: DateTime<Utc>,
    /// The microformat data
    rel_url: RelUrl<'a>,
}
