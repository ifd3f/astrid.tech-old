use std::net::SocketAddr;

use crate::db::get_db;
use crate::webmention::processing::PendingRequest;
use crate::webmention::MentionConfig;
use chrono::Utc;
use diesel::insert_into;
use rocket::form::Form;
use rocket::http::Status;
use rocket::response::status::BadRequest;
use rocket::State;

#[derive(FromForm)]
pub struct WebmentionInput<'f> {
    source: &'f str,
    target: &'f str,
}

#[post("/api/webmention", data = "<params>")]
pub fn receive_webmention(
    remote_addr: SocketAddr,
    config: &State<MentionConfig>,
    params: Form<WebmentionInput>,
) -> Result<(), BadRequest<String>> {
    use crate::schema::mentions::dsl::*;
    use diesel::prelude::*;

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
    limit: Option<i64>,
    max_retries: Option<i32>,
}

/// Schecules a task to process all the stored webmentions. This endpoint should be protected
/// and called on a cron job.
#[post("/api/rpc/processWebmentions", data = "<params>")]
pub async fn process_webmentions(params: Form<ProcessWebmentionsRequest>) -> Status {
    use crate::schema::mentions::dsl::*;
    use diesel::prelude::*;

    let limit = params.limit.unwrap_or(100);
    let max_retries = params.max_retries.unwrap_or(10);
    let db = get_db();

    let requests = mentions
        .select((
            id,
            source_url,
            target_url,
            sender_ip,
            processing_attempts,
            mentioned_on,
        ))
        .filter(processing_status.ne(2).and(processing_attempts.lt(max_retries)))
        .limit(limit)
        .load::<PendingRequest>(&db)
        .unwrap();

    for request in requests {
        request.process("webmentions").await.unwrap();
    }

    Status::Ok
}
