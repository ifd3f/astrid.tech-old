#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]


extern crate dotenv;
extern crate newbase60;
#[macro_use]
extern crate rocket;
#[macro_use]
extern crate diesel;

use dotenv::dotenv;

use crate::routes::*;

mod webmention;
pub mod schema;
mod routes;
mod db;

fn main() {
    dotenv().ok();

    rocket::ignite()
        .mount("/", routes![receive_webmention])
        .launch();
}
