use rocket::{self, form::Form, response::Redirect , response::Responder};
use crate::web::micropub::Micropub;

#[get("/<year>/<month>/<day>/<ordinal>/<slug>")]
pub async fn get_exact_post(
    year: u32,
    month: u8,
    day: u8,
    ordinal: u8,
    slug: Option<String>
) -> Redirect {
    todo!();
}

// #[post("/api/micropub")]
// pub async fn micropub_post(post: Form<Micropub>) -> impl Responder {
//     todo!();
// }