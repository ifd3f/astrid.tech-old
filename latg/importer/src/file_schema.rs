use std::{collections::HashSet, fmt};

use chrono::{DateTime, FixedOffset, Utc};
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer, Serialize,
};
use uuid::Uuid;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Document {
    pub uuid: Uuid,
    pub created_date: DateTime<FixedOffset>,
    pub published_date: DateTime<FixedOffset>,
    pub updated_date: Option<DateTime<FixedOffset>>,
    pub content: Option<Content>,
    pub tags: Vec<String>,
    pub colophon: String,
    pub doc_type: Entry,
}

#[derive(Debug, Deserialize)]
pub struct Entry {
    pub name: Option<String>,
    pub summary: Option<String>,
    pub location: Option<String>,
    pub photos: Option<Vec<String>>,
    pub reply_to: Option<Vec<String>>,
    pub repost_of: Option<String>,
    pub rsvp: Option<RSVP>,
    pub ordinal: i32,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RSVP {
    Yes,
    No,
    Maybe,
    Interested,
}

struct RSVPVisitor;

impl<'de> Visitor<'de> for RSVPVisitor {
    type Value = RSVP;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a boolean, or one of {yes, no, maybe, interested}")
    }

    fn visit_bool<E: de::Error>(self, value: bool) -> Result<Self::Value, E> {
        Ok(match value {
            true => RSVP::Yes,
            false => RSVP::No,
        })
    }

    fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
        match value {
            "yes" => Ok(RSVP::Yes),
            "no" => Ok(RSVP::Yes),
            "maybe" => Ok(RSVP::Maybe),
            "interested" => Ok(RSVP::Interested),
            value => Err(E::custom(format!("unknown RSVP: {}", value))),
        }
    }
}

impl<'de> Deserialize<'de> for RSVP {
    fn deserialize<D>(deserializer: D) -> Result<RSVP, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(RSVPVisitor)
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub enum Content {
    EmbeddedPlaintext(String),
    FileRef {
        src: Option<String>,
        downloadable: bool,
    },
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use chrono::Utc;

    use crate::file_schema::RSVP;

    #[test]
    fn rsvp_works_for_true() {
        let result: RSVP = serde_json::from_str("true").unwrap();

        assert_eq!(result, RSVP::Yes);
    }

    #[test]
    fn rsvp_works_for_str() {
        let result: RSVP = serde_json::from_str("\"interested\"").unwrap();

        assert_eq!(result, RSVP::Interested);
    }

    #[test]
    fn rsvp_fails_for_int() {
        let result: Result<RSVP, _> = serde_json::from_str("1238");

        assert_matches!(result, Err(_));
    }
}
