use std::net::SocketAddr;

use diesel::insert_into;
use rocket::{http::RawStr, request::Form, response::status::BadRequest, State};
use crate::schema::mentions::dsl::*;
use crate::diesel::RunQueryDsl;
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
    let sender: String = remote_addr.to_string();

    let mention = config
        .create_mention(params.source, params.target, sender.as_str(), 0)
        .map_err(|e| e.into())?;

    let db = get_db();
    insert_into(mentions).values(&mention).execute(&db).unwrap();

    Ok(())
}
