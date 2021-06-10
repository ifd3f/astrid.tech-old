#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]

extern crate dotenv;
#[macro_use]
extern crate rocket;

use std::env;

use dotenv::dotenv;
use rocket::http::RawStr;
use rocket_contrib::templates::Template;
use serde::{Deserialize, Serialize};

use crate::shortener::routes::TargetURL;

mod shortener;
mod projects;
mod blog;

#[derive(Serialize, Deserialize)]
struct ExampleContext<'a> {
    name: &'a str
}

#[get("/")]
fn index() -> Template {
    let context = ExampleContext { name: "memes" };
    Template::render("index", &context)
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
        .mount("/", routes![
            index,
            // projects::routes::project,
            // projects::routes::projects_index,
            shortener::routes::lengthen
        ])
        .attach(Template::fairing())
        .launch();
}
