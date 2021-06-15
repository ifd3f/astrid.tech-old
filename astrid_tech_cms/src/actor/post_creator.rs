use std::num::ParseIntError;

use actix::{Actor, Context};
use actix::prelude::*;
use chrono::{Datelike, DateTime, Utc};
use itertools::Itertools;
use uuid::Uuid;
use vfs::{VfsError, VfsPath, VfsResult};

use crate::content::{post, content};
use crate::content::post::Syndication;
use crate::web::micropub::Entry;
use crate::content::content::ContentType;

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
        let ordinal = get_next_ordinal(day_dir)?;
        let ordinal_dir = day_dir.join(ordinal.to_string().as_str())?;

        let uuid = Uuid::new_v4();
        let file = ordinal_dir.join(uuid.to_string().as_str())?;

        let short_name = "".to_string();

        let meta = post::EmbeddedMeta {
            title: msg.entry.name,
            description: msg.entry.summary,
            short_name: Some(short_name),
            uuid,
            date,
            published_date: msg.entry.published,
            updated_date: msg.entry.updated,
            ordinal,
            reply_to: msg.entry.in_reply_to,
            repost_of: msg.entry.repost_of,
            tags: msg.entry.category,
            syndications: msg.entry.mp_syndicate_to.iter()
                .map(|url| {
                    Syndication::Scheduled { url: url.clone(), strategy: None }
                })
                .collect(),
            h_type: HType::Entry,
            media: vec![],
        };

        let content = content::PostContent {
            content_type: ContentType::Markdown,
            content_path: "".to_string(),
            content: "".to_string()
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

fn get_next_ordinal(day_dir: VfsPath) -> Result<usize, CreatePostError> {
    let largest_ordinal = day_dir.read_dir()?
        .map(|x| x.filename().parse::<usize>())
        .fold_ok(0, usize::max);
    Ok(largest_ordinal? + 1)
}

impl Handler<CreatePost> for PostCreator {
    type Result = MessageResult<CreatePost>;

    fn handle(&mut self, msg: CreatePost, ctx: &mut Self::Context) -> Self::Result {
        MessageResult(self.create_post(msg.entry))
    }
}