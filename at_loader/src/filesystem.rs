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
    use std::io::{Write, Read};

    use crate::filesystem::FilesystemSet;
    use crate::test_util::{get_resources_path, get_temp_path};
    use std::borrow::Borrow;
    use std::ops::Deref;

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

    #[test]
    fn ephemeral_write_does_not_change_base() {
        let root = get_temp_path("root");
        let eph = get_temp_path("eph");
        let fss = FilesystemSet::create(root.clone(), eph.clone());
        let mut base_file_path = root.join("file.txt");
        {
            let mut base_file = fs::File::create(base_file_path.as_path()).unwrap();
            base_file.write_all("base file contents".as_ref()).unwrap();
        }
        let virtual_file_path = fss.get_filesystem().join("file.txt").unwrap();

        {
            let mut virtual_file = virtual_file_path.create_file().unwrap();
            virtual_file.write_all("overlay file contents".as_ref());
        }

        {
            let mut base_file = fs::File::open(base_file_path).unwrap();
            let mut buf = String::new();
            base_file.read_to_string(&mut buf).unwrap();
            assert_eq!(buf, "base file contents");
        }
        {
            let mut virtual_file = virtual_file_path.open_file().unwrap();
            let mut buf = String::new();
            virtual_file.read_to_string(&mut buf).unwrap();
            assert_eq!(buf, "overlay file contents");
        }
    }
}
