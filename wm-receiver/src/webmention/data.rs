use std::{collections::HashSet, path::PathBuf};

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct MentionConfig {
    pub repo_dir: PathBuf,
    pub webmention_dir: PathBuf,
    pub allowed_target_hosts: HashSet<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct RelUrl {
    pub rels: Vec<String>,
    pub text: String,
}

/// A user-friendly processed webmention, stored in the git repo..
#[derive(Serialize, Deserialize)]
pub struct WebmentionRecord {
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
