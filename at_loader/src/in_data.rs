extern crate chrono;
extern crate serde;

use chrono::DateTime;
use chrono::Utc;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ProjectStatus {
    Complete,
    WIP,
    Scrapped,
    None,
}

#[derive(Debug, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ProjectMeta {
    pub title: String,
    pub status: ProjectStatus,
    pub description: String,
    pub start_date: DateTime<Utc>,
    pub end_date: Option<DateTime<Utc>>,
    pub tags: Vec<String>,
    pub url: String,
    pub thumbnail: String,
}
