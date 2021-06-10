
use rocket::http::{RawStr, Status};
use rocket::response::Redirect;
use rocket::State;

use crate::shortener::mapper::expand_shortcode;

pub struct TargetURL {
    pub url: String,
}

#[get("/<code>")]
pub fn lengthen(target_url: State<TargetURL>, code: &RawStr) -> Result<Redirect, Status> {
    let expanded = match expand_shortcode(code) {
        Ok(path) => path,
        Err(_) => return Err(Status::NotFound),
    };

    let uri = format!("{}/{}", target_url.url, expanded);

    Ok(Redirect::to(uri))
}