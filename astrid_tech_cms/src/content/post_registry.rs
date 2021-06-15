use std::collections::{ HashMap};
use std::convert::TryFrom;

use itertools::Itertools;
use vfs::{VfsError, VfsPath};

use crate::content::post::{BarePost, PostError};

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
pub struct DateSlug {
    pub year: i32,
    pub month: u8,
    pub day: u8,
    pub ordinal: usize,
}

#[derive(Debug)]
pub enum PostRegistryError {
    Filesystem(vfs::VfsError),
    Post(PostError),
}

impl From<PostError> for PostRegistryError {
    fn from(e: PostError) -> Self {
        PostRegistryError::Post(e)
    }
}

impl From<vfs::VfsError> for PostRegistryError {
    fn from(e: VfsError) -> Self {
        PostRegistryError::Filesystem(e)
    }
}

/// Contains all of the website data
pub struct PostRegistry {
    pub map: HashMap<DateSlug, BarePost>
}

pub struct Post {

}

impl PostRegistry {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

impl TryFrom<VfsPath> for PostRegistry {
    type Error = PostRegistryError;

    fn try_from(path: VfsPath) -> Result<Self, Self::Error> {
        let posts = path.walk_dir()?
            .filter_ok(|p| p.filename().starts_with("index."))
            .map_ok(|p| BarePost::try_from(p));

        let mut map: HashMap<DateSlug, BarePost> = HashMap::new();

        for post in posts {
            let post = post??;
            map.insert(post.get_slug(), post);
        }

        Ok(PostRegistry { map })
    }
}
