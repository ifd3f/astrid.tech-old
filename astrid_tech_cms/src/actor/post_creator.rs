use std::num::ParseIntError;

use actix::{Actor, Context};
use actix::prelude::*;
use chrono::{Datelike,  Utc};
use itertools::Itertools;
use uuid::Uuid;
use vfs::{VfsError, VfsPath};

use crate::content::{post};
use crate::content::post::{HType, Syndication};
use crate::web::micropub::Entry;

pub struct PostCreator {
    filesystem: VfsPath
}

impl PostCreator {
    fn create_post(&self, entry: Entry) -> Result<(), CreatePostError> {
        let date = Utc::now();
        let day_dir = self.filesystem
            .join(date.year().to_string().as_str())?
            .join(date.month().to_string().as_str())?
            .join(date.day().to_string().as_str())?;
        let ordinal = get_next_ordinal(&day_dir)?;
        let ordinal_dir = day_dir.join(ordinal.to_string().as_str())?;

        let uuid = Uuid::new_v4();
        let file = ordinal_dir.join(uuid.to_string().as_str())?;

        let short_name = "".to_string();

        let meta = post::EmbeddedMeta {
            title: entry.name,
            description: entry.summary,
            short_name: Some(short_name),
            uuid,
            date,
            published_date: entry.published,
            updated_date: entry.updated,
            ordinal,
            reply_to: entry.in_reply_to,
            repost_of: entry.repost_of,
            tags: entry.category,
            syndications: entry.mp_syndicate_to.iter()
                .map(|url| {
                    Syndication::Scheduled { url: url.clone(), strategy: None }
                })
                .collect(),
            h_type: HType::Entry,
            media: vec![],
        };
        Ok(())
    }
}

impl Actor for PostCreator {
    type Context = Context<Self>;
}

#[derive(Message)]
#[rtype(result = "Result<(), CreatePostError>")]
struct CreatePost {
    entry: Entry
}

#[derive(Debug)]
enum CreatePostError {
    Filesystem(VfsError),
    NonNumericName(ParseIntError),
}

impl From<VfsError> for CreatePostError {
    fn from(e: VfsError) -> Self {
        CreatePostError::Filesystem(e)
    }
}

impl From<ParseIntError> for CreatePostError {
    fn from(e: ParseIntError) -> Self {
        CreatePostError::NonNumericName(e)
    }
}

fn get_next_ordinal(day_dir: &VfsPath) -> Result<usize, CreatePostError> {
    let largest_ordinal = day_dir.read_dir()?
        .map(|x| x.filename().parse::<usize>())
        .fold_ok(0, usize::max);
    Ok(largest_ordinal? + 1)
}

impl Handler<CreatePost> for PostCreator {
    type Result = MessageResult<CreatePost>;

    fn handle(&mut self, msg: CreatePost, _ctx: &mut Self::Context) -> Self::Result {
        MessageResult(self.create_post(msg.entry))
    }
}