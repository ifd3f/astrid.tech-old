
use rocket::{Response, State};
use rocket::http::{RawStr, Status};
use rocket::response::Redirect;
use rocket::response::status::NotFound;

#[get("/<y>/")]
pub fn year(y: u32) -> Redirect {
    Redirect::to(target_url.url.clone())
}

#[get("/<y>/<m>/")]
pub fn month(y: u32, m: u32) -> Redirect {
    Redirect::to(target_url.url.clone())
}

#[get("/<y>/<m>/<d>/")]
pub fn day(y: u32, m: u32, d: u32) -> Result<Redirect, Status> {
    Ok(Redirect::to(uri))
}

#[get("/<y>/<m>/<d>/<o>/")]
pub fn ordinal(y: u32, m: u32, d: u32) -> Result<Redirect, Status> {
    Ok(Redirect::to(uri))
}

#[get("/<y>/<m>/<d>/<o>/<sn>/")]
pub fn fully_qualified(y: u32, m: u32, d: u32, o: u32, sn: &RawStr) -> Result<Redirect, Status> {
    Ok(Redirect::to(uri))
}