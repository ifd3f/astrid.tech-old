use std::collections::hash_map::RandomState;

use rocket::http::{RawStr, Status};

use crate::{db::get_db, webmention::InsertMention};

#[get("/api/webmention?<source>&<target>")]
pub fn receive_webmention(source: &RawStr, target: &RawStr) -> Result<(), Status> {
    let db = get_db();

    let mention = InsertMention::new(source, target, sender, 0);

    Ok(())
}
