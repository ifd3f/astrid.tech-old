use std::num::ParseIntError;

use actix::{Actor, Context};
use actix::prelude::*;
use chrono::{Datelike, Utc};
use itertools::Itertools;
use uuid::Uuid;
use vfs::{VfsError, VfsPath};

use crate::content::post;
use crate::content::post::{HType, Post, Syndication};
use crate::content::post_registry::{PostStorage, PostStorageError};
use crate::web::micropub::Micropub;

pub struct PostCreator {
    storage: PostStorage
}

impl PostCreator {}

impl Actor for PostCreator {
    type Context = Context<Self>;
}

#[derive(Message)]
#[rtype(result = "Result<Post, PostStorageError>")]
struct CreatePost {
    mp: Micropub
}

impl Handler<CreatePost> for PostCreator {
    type Result = MessageResult<CreatePost>;

    fn handle(&mut self, msg: CreatePost, _ctx: &mut Self::Context) -> Self::Result {
        MessageResult(self.storage.create_post(msg.mp))
    }
}