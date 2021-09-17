use std::net::SocketAddr;

use rocket::{http::RawStr, request::Form, response::status::BadRequest, State};

use crate::db::get_db;
use crate::webmention::MentionConfig;

#[derive(FromForm)]
pub struct WebmentionInput<'f> {
    source: &'f RawStr,
    target: &'f RawStr,
}

#[post("/api/webmention", data = "<params>")]
pub fn receive_webmention(
    remote_addr: SocketAddr,
    config: State<MentionConfig>,
    params: Form<WebmentionInput>,
) -> Result<(), BadRequest<String>> {
    let db = get_db();
    let sender_ip = remote_addr.to_string();

    let mention = config
        .create_mention(params.source, params.target, sender_ip.as_str(), 0)
        .map_err(|e| e.into())?;

    //insert_into(mentions::table).values(&mention).execute(&db);

    Ok(())
}
