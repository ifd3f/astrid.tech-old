extern crate diesel;

use diesel::Queryable;

#[derive(Queryable)]
pub struct Project {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
}
