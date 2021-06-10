use vfs::VfsPath;
use std::convert::TryFrom;

pub struct ImageRegistry {

}

impl ImageRegistry {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

impl TryFrom<VfsPath> for ImageRegistry {
    type Error = ();

    fn try_from(value: VfsPath) -> Result<Self, Self::Error> {
        todo!()
    }
}
