#[macro_use]
extern crate diesel;

use actix_web::{App, get, HttpServer, Responder};

use crate::web::posts;


mod content;
mod web;
mod syndication;
mod actor;

#[allow(non_snake_case)]
mod schema;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(posts::get_posts))
        .bind("127.0.0.1:8080")?
        .run()
        .await
}