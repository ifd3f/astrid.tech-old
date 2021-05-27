#![feature(decl_macro)]
#![feature(proc_macro_hygiene)]
#![feature(in_band_lifetimes)]

extern crate dotenv;
#[macro_use]
extern crate rocket;

use std::env;

use dotenv::dotenv;
use rocket_contrib::templates::Template;
use serde::{Serialize, Deserialize};
use crate::shortener::routes::TargetURL;

mod shortener;

#[derive(Serialize, Deserialize)]
struct ExampleContext<'a> {
    name: &'a str
}

#[get("/")]
fn index() -> Template {
    let context = ExampleContext{
        name: "memes"
    };
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
        .mount("/", routes![index, shortener::routes::lengthen])
        .attach(Template::fairing())
        .launch();
}
