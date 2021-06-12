use chrono::{DateTime, Duration, Utc};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(tag = "h")]
pub enum Micropub {
    #[serde(rename = "entry")]
    Entry { content: String, category: Vec<String>, mp_syndicate_to: Vec<String> }
}


#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub struct Entry {
    name: Option<String>,
    summary: Option<String>,
    content: Option<String>,
    published: Option<DateTime<Utc>>,
    updated: Option<DateTime<Utc>>,
    category: Vec<String>,
    location: String,
    in_reply_to: String,
    repost_of: Vec<String>,
    syndication: Vec<String>,
    mp_syndicate_to: String,
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

