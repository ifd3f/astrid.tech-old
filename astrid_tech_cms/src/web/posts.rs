use actix_web::{get, post, Responder};
use actix_web::web::Path;

#[get("/api/posts")]
pub async fn index() -> impl Responder {
    todo!();
    ""
}

#[post("/api/posts")]
pub async fn post() -> impl Responder {
    todo!();
    ""
}