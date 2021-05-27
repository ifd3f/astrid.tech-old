#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]

extern crate dotenv;
#[macro_use]
extern crate rocket;

use std::env;
use dotenv::dotenv;
mod shortener_routes;
use crate::shortener_routes::TargetURL;


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
        .mount("/", routes![shortener_routes::index, shortener_routes::lengthen])
        .launch();
}
