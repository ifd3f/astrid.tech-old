use std::convert::TryInto;

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::content::content::PostContent;
use crate::content::post::BarePost;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(tag = "h")]
pub enum Micropub {
    #[serde(rename = "entry")]
    Entry(Entry)
}


#[serde(rename = "entry", rename_all = "kebab-case")]
pub struct Entry {
    pub name: Option<String>,
    pub summary: Option<String>,
    pub content: Option<String>,
    pub published: Option<DateTime<Utc>>,
    pub updated: Option<DateTime<Utc>>,
    #[serde(default)]
    pub category: Vec<String>,
    pub location: String,
    pub in_reply_to: Option<Url>,
    pub repost_of: Option<Url>,
    #[serde(default)]
    pub syndication: Vec<Url>,
    #[serde(default)]
    pub mp_syndicate_to: Vec<Url>,
}


#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub struct Event {
    name: Option<String>,
    summary: Option<String>,
    description: Option<String>,
    #[serde(flatten)]
    time: EventTime,
    duration: String,
    category: Vec<String>,
    location: String,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(untagged)]
enum EventTime {
    StartOnly { start: DateTime<Utc> },
    //StartDuration { start: DateTime<Utc>, duration: Duration },
    StartEnd { start: DateTime<Utc>, end: DateTime<Utc> },
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub struct Cite {
    name: Option<String>,
    published: Option<DateTime<Utc>>,
    author: Option<String>,
    url: Option<String>,
    content: Option<String>,
}

#[cfg(test)]
mod tests {
    #[test]
    fn parses_micropub(){
        const H_ENTRY = r#"{
            "name": "test"
        }"#;
    }
}