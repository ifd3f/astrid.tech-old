use std::collections::HashMap;
use std::convert::TryFrom;
use std::num::ParseIntError;

use chrono::{Datelike, Utc, DateTime};
use itertools::Itertools;
use uuid::Uuid;
use vfs::{VfsError, VfsPath};

use crate::content::post::{Post, PostError};
use crate::web::micropub::Micropub;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
pub struct DateSlug {
    pub year: i32,
    pub month: u8,
    pub day: u8,
    pub ordinal: u32,
}

#[derive(Debug)]
pub enum PostStorageError {
    Filesystem(vfs::VfsError),
    Post(PostError),
    NonNumericOrdinal(ParseIntError),
}

impl From<PostError> for PostStorageError {
    fn from(e: PostError) -> Self {
        PostStorageError::Post(e)
    }
}

impl From<vfs::VfsError> for PostStorageError {
    fn from(e: VfsError) -> Self {
        PostStorageError::Filesystem(e)
    }
}

impl From<ParseIntError> for PostStorageError {
    fn from(e: ParseIntError) -> Self {
        PostStorageError::NonNumericOrdinal(e)
    }
}

/// Contains all of the website data
pub struct PostStorage {
    filesystem: VfsPath
}

fn get_next_ordinal(day_dir: &VfsPath) -> Result<u32, PostStorageError> {
    let largest_ordinal = day_dir.read_dir()?
        .map(|x| x.filename().parse::<u32>())
        .fold_ok(0, u32::max);
    Ok(largest_ordinal? + 1)
}

impl PostStorage {
    pub fn create_post(&self, mp: Micropub) -> Result<Post, PostStorageError> {
        let date = Utc::now();
        let day_dir = self.filesystem
            .join(date.year().to_string().as_str())?
            .join(date.month().to_string().as_str())?
            .join(date.day().to_string().as_str())?;

        let ordinal = get_next_ordinal(&day_dir)?;
        let mut ordinal_dir = day_dir.join(ordinal.to_string().as_str())?;

        let uuid = Uuid::new_v4();

        let post = Post::from_micropub(uuid, DateTime::from(date), ordinal, mp);
        ordinal_dir.create_dir_all()?;
        post.write_to(&mut ordinal_dir)?;

        Ok(post)
    }

    pub fn iter_posts(&self) -> impl Iterator<Item=Result<Post, PostError>> {
        self.filesystem
            .read_dir().unwrap()  // years
            .flat_map(|d| d.read_dir().unwrap())  // months
            .flat_map(|d| d.read_dir().unwrap())  // days
            .flat_map(|d| d.read_dir().unwrap())  // ordinals
            .map(|d| Post::try_from(d))
    }
}

impl From<VfsPath> for PostStorage {
    fn from(filesystem: VfsPath) -> Self {
        PostStorage { filesystem }
    }
}