#![feature(proc_macro_hygiene, decl_macro)]
#![feature(in_band_lifetimes)]

extern crate dotenv;
#[macro_use]
extern crate rocket;

use std::env;

use dotenv::dotenv;
use rocket::http::{RawStr, Status};
use rocket::response::Redirect;

use mapper::expand_shortcode;
use rocket::response::status::NotFound;
use rocket::Response;

mod mapper;

#[get("/")]
fn index() -> Redirect {
    Redirect::to("http://astrid.tech")
}

#[get("/<code>")]
fn lengthen(code: &RawStr) -> Result<Redirect, Status> {
    let expanded = match expand_shortcode(code) {
        Ok(path) => path,
        Err(_) => return Err(Status::NotFound),
    };

    let uri = format!("{}/{}", "http://astrid.tech", expanded);

    Ok(Redirect::to(uri))
}

fn main() {
    dotenv().ok();

    rocket::ignite()
        .mount("/", routes![index, lengthen])
        .launch();
}
