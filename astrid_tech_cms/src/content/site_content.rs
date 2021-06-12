use std::collections::HashMap;
use std::convert::TryFrom;

use vfs::VfsPath;

use crate::content::media::Media;
use crate::content::post_registry::PostRegistry;

/// Contains all of the website data
pub struct BlogContent {
    media: HashMap<String, Media>,
    posts: PostRegistry
}

impl BlogContent {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

impl TryFrom<VfsPath> for BlogContent {
    type Error = ();

    fn try_from(path: VfsPath) -> Result<Self, Self::Error> {
        let posts = PostRegistry::try_from(path).unwrap();

        // Load non-post media
        todo!()
    }
}