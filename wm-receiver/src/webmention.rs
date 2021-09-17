use rocket::response::status::BadRequest;
use std::{collections::HashSet, fmt::Display};
use url::Url;

use crate::schema::mentions;

#[derive(Debug)]
pub struct MentionConfig {
    pub allowed_target_hosts: HashSet<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum MentionRequestError<'a> {
    InvalidURL(&'a str),
    UnknownTargetHost(String),
}

#[inline(always)]
fn validate_url(
    url: &'a str,
    allowed_target_hosts: Option<&HashSet<String>>,
) -> Result<(), MentionRequestError<'a>> {
    let parsed = Url::parse(url).map_err(|_| MentionRequestError::InvalidURL(url))?;

    if !(parsed.scheme() == "http" || parsed.scheme() == "https") {
        Err(MentionRequestError::InvalidURL(url))?;
    }

    // If a set of allowed hosts is provided
    if let Some(host_set) = allowed_target_hosts {
        let host = parsed.host().ok_or(MentionRequestError::InvalidURL(url))?;
        let host = host.to_string();
        if !host_set.contains(&host) {
            Err(MentionRequestError::UnknownTargetHost(host))?;
        }
    }

    Ok(())
}

impl MentionConfig {
    pub fn create_mention(
        &self,
        source_url: &'a str,
        target_url: &'a str,
        sender_ip: &'a str,
        processing_status: i32,
    ) -> Result<InsertMention<'a>, MentionRequestError<'a>> {
        validate_url(source_url, None)?;
        validate_url(target_url, Some(&self.allowed_target_hosts))?;

        Ok(InsertMention {
            source_url,
            target_url,
            sender_ip,
            processing_status,
        })
    }
}

impl<'a> Display for MentionRequestError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MentionRequestError::InvalidURL(url) => write!(f, "Invalid URL: {}", url),
            MentionRequestError::UnknownTargetHost(host) => {
                write!(f, "Unknown host for target param: {}", host)
            }
        }
    }
}

impl<'a> Into<BadRequest<String>> for MentionRequestError<'a> {
    fn into(self) -> BadRequest<String> {
        BadRequest(Some(self.to_string()))
    }
}

#[derive(Insertable, Debug)]
#[table_name = "mentions"]
pub struct InsertMention<'a> {
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_status: i32,
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::webmention::MentionRequestError;

    use super::MentionConfig;

    fn get_config() -> MentionConfig {
        MentionConfig {
            allowed_target_hosts: vec![
                "allowed.example.com".to_string(),
                "another.example.com".to_string(),
            ]
            .into_iter()
            .collect(),
        }
    }

    #[test]
    fn request_allowed_host_should_pass() {
        let config = get_config();

        let source = "https://someone.example.net/their/article";
        let target = "https://allowed.example.com/our/article";
        let sender = "1.2.3.4";

        let result = config.create_mention(source, target, sender, 0).unwrap();

        assert_eq!(result.target_url, target)
    }

    #[test]
    fn request_unknown_host_should_error() {
        let config = get_config();

        let source = "https://someone.example.net/their/article";
        let target = "https://facebook.com/our/article";
        let sender = "1.2.3.4";

        let result = config.create_mention(source, target, sender, 0);

        assert_matches!(result, Err(MentionRequestError::UnknownTargetHost(..)));
    }

    #[test]
    fn request_invalid_protocol_should_error() {
        let config = get_config();

        let source = "gopher://someone.example.net/their/article";
        let target = "https://allowed.example.com/our/article";
        let sender = "1.2.3.4";

        let result = config.create_mention(source, target, sender, 0);

        assert_matches!(result, Err(MentionRequestError::InvalidURL(..)));
    }
}
