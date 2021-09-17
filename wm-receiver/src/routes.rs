use std::net::SocketAddr;

use rocket::{
    http::{RawStr, Status},
    Request, State,
};

use crate::{db::get_db, webmention::MentionConfig};

#[get("/api/webmention?<source>&<target>")]
pub fn receive_webmention(
    remote_addr: SocketAddr,
    config: State<MentionConfig>,
    source: &RawStr,
    target: &RawStr,
) -> Result<(), Status> {
    let db = get_db();
    let sender_ip = remote_addr.to_string();

    let mention = config.create_mention(source, target, sender_ip.as_str(), 0);

    Ok(())
}
