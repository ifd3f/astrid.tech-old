#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]
#![feature(assert_matches)]

extern crate dotenv;
#[macro_use]
extern crate rocket;
#[macro_use]
extern crate diesel;

use std::env;

use dotenv::dotenv;
use webmention::MentionConfig;

use crate::{db::get_db, routes::*};

mod db;
mod routes;
mod schema;
mod webmention;

fn main() {
    dotenv().ok();

    let target_hosts = env::var("ALLOWED_TARGET_HOSTS").expect("ALLOWED_TARGET_HOSTS must be set");

    // Ensure the database can be connected to
    get_db();

    // Generate mention config
    let mention_config = MentionConfig {
        allowed_target_hosts: target_hosts
            .split(",")
            .map(|host| host.to_owned())
            .collect(),
    };

    rocket::ignite()
        .manage(mention_config)
        .mount("/", routes![receive_webmention])
        .launch();
}
