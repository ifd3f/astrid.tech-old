use actix_web::{get, App, HttpServer, Responder};
use crate::web::posts;

mod content;
mod web;
mod syndication;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(posts::index))
        .bind("127.0.0.1:8080")?
        .run()
        .await
}