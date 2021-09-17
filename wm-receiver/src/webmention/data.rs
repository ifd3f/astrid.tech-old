use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct RelUrl {
    pub rels: Vec<String>,
    pub text: String,
}

pub enum MentionProcessingStatus {
    Unprocessed,
    Failed,
    Succeeded,
}

/// A user-friendly processed webmention.
#[derive(Serialize, Deserialize)]
pub struct Webmention {
    /// The normalized URL of the source that sent the webmention.
    pub source_url: String,
    /// The normalized URL of the target that is mentioned.
    pub target_url: String,
    /// When this mention was sent.
    pub mentioned_on: DateTime<Utc>,
    /// When this mention was processed.
    pub processed_on: DateTime<Utc>,
    /// The microformat data
    pub rel_url: RelUrl,
}
