#[macro_use]
extern crate rocket;

use crate::web::posts;

mod content;
mod syndication;
mod web;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index])
}
