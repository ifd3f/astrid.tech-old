#[macro_use]
extern crate rocket;

use crate::web::posts;

mod syndication;
mod web;

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![web::index::index])
}
