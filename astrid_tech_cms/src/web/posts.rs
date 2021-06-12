use actix_web::web::Path;
use actix_web::{get,Responder};

#[get("/api/posts")]
pub async fn index() -> impl Responder {
    todo!();
    ""
}