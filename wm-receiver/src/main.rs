#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]
#![feature(assert_matches)]

extern crate dotenv;
extern crate serde_json;
extern crate sha256;
#[macro_use]
extern crate diesel_migrations;
#[cfg(test)]
extern crate tempdir;
#[macro_use]
extern crate rocket;
#[macro_use]
extern crate diesel;

use std::{env, path::PathBuf};

use dotenv::dotenv;
use webmention::data::MentionConfig;

use crate::{db::run_migrations, routes::*};

mod db;
mod routes;
mod schema;
mod webmention;

#[inline]
fn require_env(name: &str, default: Option<&str>) -> String {
    if let Some(var) = env::var(name).ok() {
        return var;
    }
    if let Some(data) = default {
        return data.to_string();
    }
    panic!("{} was not provided!", name);
}

#[launch]
fn rocket() -> _ {
    dotenv().ok();

    // Perform diesel migrations
    run_migrations();

    let allowed_target_hosts = require_env("ALLOWED_TARGET_HOSTS", None)
        .split(",")
        .map(|host| host.to_owned())
        .collect();
    let remote_url = require_env("REMOTE_URL", None);
    let webmention_subdir = PathBuf::from(require_env("WEBMENTION_SUBDIR", None));

    let repo_dir = PathBuf::from(require_env("REPO_DIR", Some("/var/wm-receiver/repo")));
    let branch_name = require_env("BRANCH_NAME", Some("webmention/"));
    let base_branch = require_env("BASE_BRANCH", Some("webmention/"));

    let webmention_dir = repo_dir.join(webmention_subdir);

    // Generate mention config
    let mention_config = MentionConfig {
        repo_dir,
        webmention_dir,
        allowed_target_hosts,
        remote_url,
        branch_name,
        base_branch,
    };

    rocket::build()
        .manage(mention_config)
        .mount("/", routes![receive_webmention, process_webmentions])
}
