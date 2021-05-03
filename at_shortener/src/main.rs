#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]

extern crate dotenv;
extern crate newbase60;
#[macro_use]
extern crate rocket;

use std::env;

use dotenv::dotenv;
use rocket::http::{RawStr, Status};
use rocket::response::status::NotFound;
use rocket::response::Redirect;
use rocket::{Response, State};

use mapper::expand_shortcode;

mod follow_redirect;
mod mapper;

struct TargetURL {
    url: String,
}

#[get("/")]
fn index(target_url: State<TargetURL>) -> Redirect {
    Redirect::to(target_url.url.clone())
}

#[get("/<code>")]
fn lengthen(target_url: State<TargetURL>, code: &RawStr) -> Result<Redirect, Status> {
    let expanded = match expand_shortcode(code) {
        Ok(path) => path,
        Err(_) => return Err(Status::NotFound),
    };

    let uri = format!("{}/{}", target_url.url, expanded);

    Ok(Redirect::to(uri))
}

fn main() {
    dotenv().ok();

    let target_url = TargetURL {
        url: match env::var("TARGET_URL") {
            Ok(url) => url,
            _ => "https://astrid.tech".to_string(),
        },
    };

    rocket::ignite()
        .manage(target_url)
        .mount("/", routes![index, lengthen])
        .launch();
}
