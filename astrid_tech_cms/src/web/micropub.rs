use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde::de::Error;
use url::Url;

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "h", rename_all = "kebab-case")]
pub enum Micropub {
    Entry(Entry)
}

/// Needed because it's weird
#[derive(Serialize, Deserialize)]
struct MicropubWorkaround {
    #[serde(flatten)]
    x: Micropub
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Entry {
    pub name: Option<String>,
    pub summary: Option<String>,
    pub content: Option<String>,
    pub published: Option<DateTime<Utc>>,
    pub updated: Option<DateTime<Utc>>,
    #[serde(default)]
    pub category: Vec<String>,
    pub location: Option<Url>,
    pub in_reply_to: Option<Url>,
    pub repost_of: Option<Url>,
    #[serde(default)]
    pub syndication: Vec<Url>,
    #[serde(default)]
    pub mp_syndicate_to: Option<Url>,
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
    use serde::{Deserialize, Serialize};

    use crate::web::micropub::{Micropub, MicropubWorkaround};

    #[test]
    fn parses_entry() {
        // from https://indieweb.org/Micropub#New_Note
        const H_ENTRY: &str = "h=entry\
&content=The+%40Jawbone+UP%2C+my+favorite+of+the+%23quantifiedself+trackers%2C+finally+released+their+official+API%21+http%3A%2F%2Fjawbone.com%2Fup%2Fdeveloper%2F\
&category[]=jawbone&category[]=quantifiedself&category[]=api\
&mp-syndicate-to=https://myfavoritesocialnetwork.example/aaronpk";

        let obj: MicropubWorkaround = serde_qs::from_str(H_ENTRY).unwrap();

        if let Micropub::Entry(data) = obj.x {
            assert_eq!(data.category, vec!["jawbone", "quantifiedself", "api"]);
        } else {
            panic!("Not an entry")
        }
    }
}