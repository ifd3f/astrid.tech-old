use actix_web::{get, post, Responder};
use actix_web::web::{Form,  Query};

use crate::web::micropub::Micropub;

#[get("/api/posts")]
pub async fn get_posts(
    year: Query<Option<isize>>,
    month: Query<Option<u8>>,
    day: Query<Option<u8>>,
    ordinal: Query<Option<u8>>,
) -> impl Responder {
    todo!();
    ""
}

#[post("/api/micropub")]
pub async fn micropub_post(post: Form<Micropub>) -> impl Responder {
    match post.0 {
        Micropub::Entry(e)=> {

        }
    }
    ""
}