use actix_web::{get, post, Responder};
use actix_web::web::{Path, Form};
use crate::web::micropub::Micropub;

#[get("/api/posts")]
pub async fn index() -> impl Responder {
    todo!();
    ""
}

#[post("/api/micropub")]
pub async fn micropub_post(post: Form<Micropub>) -> impl Responder {
    match post.0 {
        Micropub::Entry { .. } => {}
    }
    ""
}