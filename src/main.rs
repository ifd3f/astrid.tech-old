#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]

extern crate dotenv;
#[macro_use]
extern crate rocket;

use std::env;
use dotenv::dotenv;
mod shortener;

use crate::shortener::routes::TargetURL;


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
        .mount("/", routes![shortener::routes::index, shortener::routes::lengthen])
        .launch();
}
