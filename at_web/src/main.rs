#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]

mod routes;

extern crate at_shortener;
extern crate at_objects;
extern crate at_loader;
extern crate dotenv;
extern crate newbase60;
#[macro_use]
extern crate rocket;

use std::env;
use dotenv::dotenv;
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
        .mount("/", routes![shortener::index, shortener::lengthen])
        .launch();
}
