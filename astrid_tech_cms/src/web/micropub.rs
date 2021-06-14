use std::convert::TryInto;

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use url::Url;

use crate::content::post::BarePost;
use crate::content::content::PostContent;

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(tag = "h")]
pub enum Micropub {
    #[serde(rename = "entry", rename_all = "kebab-case")]
    Entry {
        name: Option<String>,
        summary: Option<String>,
        content: Option<String>,
        published: Option<DateTime<Utc>>,
        updated: Option<DateTime<Utc>>,
        #[serde(default)]
        category: Vec<String>,
        location: String,
        in_reply_to: Url,
        repost_of: Option<String>,
        #[serde(default)]
        syndication: Vec<String>,
        #[serde(default)]
        mp_syndicate_to: Vec<String>,
    }
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

