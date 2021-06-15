use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize, Deserializer};
use serde::de::Error;
use url::Url;

pub fn maybe_vec<'de, D, V>(deserializer: D) -> Result<Vec<V>, D::Error>
    where
        D: Deserializer<'de>,
        V: Deserialize<'de>,
{
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum MaybeVec<V> {
        Value(V),
        Vec(Vec<V>),
    }

    match MaybeVec::deserialize(deserializer)? {
        MaybeVec::Value(v) => Ok(vec![v]),
        MaybeVec::Vec(v) => Ok(v),
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "h", rename_all = "kebab-case")]
pub enum Micropub {
    Entry(Entry),
    Event(Event),
    Cite(Cite)
}

/// Needed because it's weird
#[derive(Deserialize, Debug)]
struct MicropubWorkaround {
    #[serde(flatten)]
    data: Micropub
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub struct Entry {
    pub name: Option<String>,
    pub summary: Option<String>,
    pub content: Option<String>,
    pub published: Option<DateTime<Utc>>,
    pub updated: Option<DateTime<Utc>>,
    #[serde(default)]
    #[serde(deserialize_with = "maybe_vec")]
    pub category: Vec<String>,
    pub location: Option<Url>,
    pub in_reply_to: Option<Url>,
    pub repost_of: Option<Url>,
    #[serde(default)]
    pub syndication: Vec<Url>,
    #[serde(default)]
    #[serde(deserialize_with = "maybe_vec")]
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
    #[serde(deserialize_with = "maybe_vec")]
    category: Vec<String>,
    location: String,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(untagged)]
enum EventTime {
    StartOnly { start: DateTime<Utc> },
    // TODO need support https://github.com/chronotope/chrono/issues/117#issuecomment-854858641
    // StartDuration { start: DateTime<Utc>, duration: chrono::Duration },
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
    use url::Url;

    use crate::web::micropub::{Micropub, MicropubWorkaround};

    #[test]
    fn parses_h_entry() {
        // from https://indieweb.org/Micropub#New_Note
        const H_ENTRY: &str = "h=entry\
&content=The+%40Jawbone+UP%2C+my+favorite+of+the+%23quantifiedself+trackers%2C+finally+released+their+official+API%21+http%3A%2F%2Fjawbone.com%2Fup%2Fdeveloper%2F\
&category[]=jawbone&category[]=quantifiedself&category[]=api\
&mp-syndicate-to=https://myfavoritesocialnetwork.example/aaronpk";

        let obj: MicropubWorkaround = serde_qs::from_str(H_ENTRY).unwrap();

        if let Micropub::Entry(data) = obj.data {
            assert_eq!(data.name, None);
            assert_eq!(data.category, vec!["jawbone", "quantifiedself", "api"]);
            assert_eq!(data.mp_syndicate_to, vec![Url::parse("https://myfavoritesocialnetwork.example/aaronpk").unwrap()]);
        } else {
            panic!("Not an entry")
        }
    }

    #[test]
    fn parses_h_event() {
        // from https://indieweb.org/Micropub#h-event
        const H_EVENT: &str = "h=event\
&name=IndieWeb Dinner at 21st Amendment\
&description=In SF Monday evening? Join @caseorganic and I for an #indieweb dinner at 6pm! (Sorry for the short notice!)\
&start=2013-09-30T18:00:00-07:00\
&category=indieweb\
&location=http://21st-amendment.com/";

        let obj: MicropubWorkaround = serde_qs::from_str(H_EVENT).unwrap();

        if let Micropub::Event(data) = obj.data {
            assert_eq!(data.name, Some("IndieWeb Dinner at 21st Amendment".to_string()));
            assert_eq!(data.category, vec!["indieweb"]);
        } else {
            panic!("Not an event")
        }
    }
}