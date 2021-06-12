use serde::__private::TryFrom;
use url::Url;

pub enum Target {
    Twitter { handle: String },
    Mastodon { user: String, instance: String },
}

impl TryFrom<Url> for Target {
    type Error = ();

    fn try_from(url: Url) -> Result<Self, Self::Error> {
        if url.scheme() == "https" && url.host_str() == Some("twitter.com") {
            let mut x = url.path_segments().ok_or(())?;
            return match (x.next(), x.next()) {
                (Some(handle), None) => Ok(Target::Twitter { handle: handle.to_string() }),
                _ => Err(())
            };
        }
        if url.scheme() == "mastodon" {
            return match (url.username(), url.host_str()) {
                ("", _) => Err(()),  // Username cannot be empty
                (_, None) => Err(()),  // Host cannot be empty
                (user, Some(host)) => {
                    Ok(Target::Mastodon {
                        user: user.to_string(),
                        instance: host.to_string(),
                    })
                }
            };
        }
        Err(())?
    }
}