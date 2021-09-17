use std::net::SocketAddr;

use crate::db::get_db;
use crate::diesel::RunQueryDsl;
use crate::schema::mentions::dsl::*;
use crate::webmention::MentionConfig;
use chrono::Utc;
use diesel::insert_into;
use rocket::{http::RawStr, request::Form, response::status::BadRequest, State};

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
    let now = Utc::now();

    let mention = config
        .create_mention(params.source, params.target, sender.as_str(), 0, now)
        .map_err(|e| e.into())?;

    let db = get_db();
    insert_into(mentions).values(&mention).execute(&db).unwrap();

    Ok(())
}

#[derive(FromForm)]
pub struct ProcessWebmentionsRequest {
    limit: Option<u32>
}

/// Schecules a task to process all the stored webmentions. This endpoint should be protected
/// and called on a cron job.
#[post("/api/rpc/processWebmentions", data = "<params>")]
pub async fn process_webmentions(params: Form<ProcessWebmentionsRequest>) -> () {
    ()
}