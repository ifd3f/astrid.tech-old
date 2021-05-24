use std::path::{PathBuf, Path};

use vfs::{PhysicalFS, VfsPath};
use vfs::impls::overlay::OverlayFS;
use std::borrow::Borrow;

pub struct FilesystemSet {
    actual_overlay_path: PathBuf,
    overlay: VfsPath,
}

impl FilesystemSet {
    pub fn create(root: PathBuf, ephemeral: PathBuf) -> FilesystemSet {
        let user_source = PhysicalFS::new(root);
        let cache = PhysicalFS::new(ephemeral.clone());

        let overlay = OverlayFS::new(&[VfsPath::new(cache), VfsPath::new(user_source)]);

        FilesystemSet {
            actual_overlay_path: ephemeral,
            overlay: VfsPath::new(overlay),
        }
    }

    pub fn get_filesystem(&self) -> &VfsPath {
        &self.overlay
    }

    pub fn get_raw_write_path(&self) -> &Path {
        self.actual_overlay_path.borrow()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::io::Write;

    use crate::filesystem::FilesystemSet;
    use crate::test_util::{get_resources_path, get_temp_path};

    #[test]
    fn raw_write_applies_to_ephemeral() {
        let fss = FilesystemSet::create(get_resources_path("."), get_temp_path("rwatt"));
        let child_file = fss.get_raw_write_path().join("foo.txt");

        let mut file = fs::File::create(&child_file).unwrap();
        file.write_all("foo bar spam".as_ref());

        let virtual_path = fss.get_filesystem().join("foo.txt").unwrap();
        let contents = virtual_path.read_to_string().unwrap();
        assert_eq!(contents, "foo bar spam");
    }
}
