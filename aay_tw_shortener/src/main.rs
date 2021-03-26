#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use]
extern crate rocket;
use rocket::http::RawStr;
use rocket::response::Redirect;

#[get("/")]
fn index() -> Redirect {
    Redirect::to("http://astrid.tech")
}

#[get("/<code>")]
fn lengthen(code: &RawStr) -> Redirect {
    "Hello, world!"
}

fn main() {
    rocket::ignite()
        .mount("/", routes![index, lengthen])
        .launch();
}
