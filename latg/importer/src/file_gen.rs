use tokio::io::AsyncWrite;
use vfs::FileSystem;

pub trait StorageBackend {
    fn get_url(filename: String) -> String;
    fn upload(vfs: VfsPath) -> Result<(), ()>;
}

/**
 * A helper for the generated files that the CMS may make during import.
 */
pub struct GeneratedFileRegistry {
    backend: Box<dyn StorageBackend>,
    local_cache: VfsPath,
}

/**
 * A reference to a generated file.
 */
pub struct GeneratedFileRef {
    pub url: String,
    pub writer: Write,
}

impl GeneratedFileRegistry {
    pub fn new(backend: Box<dyn StorageBackend>, tmp_storage: VfsPath) -> GeneratedFileRegistry {
        GeneratedFileRegistry {
            backend,
            local_cache: tmp_storage,
        }
    }

    pub fn new_file(&mut self, filename: &str) -> GeneratedFileRef {
        let dst_file = self.stored.join(filename);
        if dst_file.exists(path) {
            return self.new_file(format!("{}-", filename));
        }

        let writer = dst_file.create_file();

        GeneratedFileRef {
            url: self.backend.get_url(filename),
            writer,
        }
    }
}

pub struct FSBackend {
    url_root: String,
    phys_fs_root: VfsPath,
}

impl FSBackend {
    pub fn new(url_root: String, fs_root: PathBuf) -> FSBackend {
        FSBackend {
            url_root,
            phys_fs_root: VfsPath::new(PhysicalFS::new(fs_root)),
        }
    }
}

impl StorageBackend for FSBackend {
    fn get_url(filename: String) -> String {
        format!("{}/{}", url_root, filename)
    }

    fn upload(&self, vfs: VfsPath) -> Result<(), ()> {
        vfs.copy_dir(&self.phys_fs_root)?
    }
}
