use vfs::VfsPath;
use std::convert::TryFrom;
use std::collections::BTreeMap;
use crate::content::media::Media;

/// Contains all of the website data
pub struct PostRegistry {

}

impl PostRegistry {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

impl TryFrom<VfsPath> for PostRegistry {
    type Error = ();

    fn try_from(value: VfsPath) -> Result<Self, Self::Error> {
        // Identify potential post folders

        // Load posts from those folders, or error if it doesn't work
        todo!()
    }
}
