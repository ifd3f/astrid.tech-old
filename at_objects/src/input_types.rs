use chrono::{DateTime, NaiveDate, Utc};
use serde;

use serde::{Deserialize, Serialize};
use crate::input_types::ProjectStatus::NoStatus;

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct ArticleMeta {
    title: String,
    description: String,
    date: DateTime<Utc>,
    thumbnail: Option<String>,
    tags: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
enum ProjectStatus {
    NoStatus,
    Early,
    WIP,
    Complete,
    Scrapped,
}

impl Default for ProjectStatus {
    fn default() -> Self { NoStatus }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct ProjectMeta {
    title: String,
    status: ProjectStatus,
    start_date: NaiveDate,
    end_date: NaiveDate,
    url: Option<String>,
    source: Vec<String>,
    tags: Vec<String>,
    thumbnail: Option<String>,
    description: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct TagOverrideTargets {
    slug: String,
    name: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct TagOverride {
    background_color: String,
    color: String,
    tags: Vec<TagOverrideTargets>,
}