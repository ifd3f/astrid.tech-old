#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]
#![feature(assert_matches)]

extern crate dotenv;
extern crate serde_yaml;
extern crate slug;
#[cfg(test)]
extern crate tempdir;
#[macro_use]
extern crate rocket;
#[macro_use]
extern crate diesel;

use std::{env, path::PathBuf};

use dotenv::dotenv;
use webmention::requesting::MentionConfig;

use crate::{db::get_db, routes::*};

mod db;
mod routes;
mod schema;
mod webmention;

#[launch]
fn rocket() -> _ {
    dotenv().ok();

    // Ensure the database can be connected to
    get_db();

    let target_hosts = env::var("ALLOWED_TARGET_HOSTS").expect("ALLOWED_TARGET_HOSTS must be set");
    let repo_dir = PathBuf::from(env::var("REPO_DIR").expect("REPO_DIR must be set"));

    let webmention_subdir =
        PathBuf::from(env::var("WEBMENTION_SUBDIR").expect("WEBMENTION_SUBDIR must be set"));

    let webmention_dir = repo_dir.join(webmention_subdir);

    // Generate mention config
    let mention_config = MentionConfig {
        repo_dir,
        webmention_dir,
        allowed_target_hosts: target_hosts
            .split(",")
            .map(|host| host.to_owned())
            .collect(),
    };

    rocket::build()
        .manage(mention_config)
        .mount("/", routes![receive_webmention, process_webmentions])
}
