use rocket::{http::Status};

#[post("/api/webmention")]
pub fn receive_webmention() -> Result<(), Status> {
    Ok(())
}
