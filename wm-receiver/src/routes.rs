use std::net::SocketAddr;

use crate::db::get_db;
use crate::webmention::processing::PendingRequest;
use crate::webmention::requesting::MentionConfig;
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
    use crate::schema::requests::dsl::*;
    use diesel::prelude::*;

    let sender: String = remote_addr.to_string();
    let now = Utc::now();

    let mention = config
        .create_mention(params.source, params.target, sender.as_str(), 0, now)
        .map_err(|e| e.into())?;

    let db = get_db();
    insert_into(requests).values(&mention).execute(&db).unwrap();

    Ok(())
}

/// Schecules a task to process all the stored webmentions. This endpoint should be protected
/// and called on a cron job.
#[post("/api/rpc/processWebmentions?<limit>")]
pub async fn process_webmentions(config: &State<MentionConfig>, limit: Option<i64>) -> Status {
    use crate::schema::requests::dsl::*;
    use diesel::prelude::*;

    let limit = limit.unwrap_or(100);
    let max_retries = 10; // TODO
    let db = get_db();

    let pending_requests = requests
        .select((
            id,
            source_url,
            target_url,
            processing_attempts,
            mentioned_on,
        ))
        .filter(
            processing_status
                .ne(2)
                .and(processing_attempts.lt(max_retries)),
        )
        .limit(limit)
        .load::<PendingRequest>(&db)
        .unwrap();

    for request in pending_requests {
        request.process(&config.webmention_dir).await.unwrap();
    }

    Status::Ok
}
