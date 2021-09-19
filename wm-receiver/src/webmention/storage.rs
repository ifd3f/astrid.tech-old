use std::error::Error;
use std::fs::{self, File};
use std::path::PathBuf;

use super::data::WebmentionRecord;
use sanitize_filename::sanitize_with_options;

pub struct WebmentionStore {
    path: PathBuf,
}

impl WebmentionStore {
    pub fn new(path: PathBuf) -> WebmentionStore {
        WebmentionStore { path }
    }

    pub fn get_webmention(
        &self,
        source_url: impl AsRef<str>,
        target_url: impl AsRef<str>,
    ) -> Option<WebmentionRecord> {
        let mut path = PathBuf::new();
        path.push(&self.path);
        append_storage_subpath(&mut path, source_url, target_url);

        let file = match File::open(path) {
            Ok(file) => file,
            Err(_) => return None,
        };
        serde_json::from_reader(file).ok()
    }

    pub fn apply(&mut self, action: StorageAction) -> Result<(), Box<dyn Error>> {
        let path = {
            let mut path = PathBuf::new();
            path.push(&self.path);
            action.append_storage_subpath(&mut path);
            path
        };

        // Safe to unwrap because this is definitely a subdirectory
        let parent = path.parent().unwrap();
        fs::create_dir_all(&parent)?;

        match action {
            StorageAction::Delete { .. } => {
                fs::remove_file(path)?;
            }
            StorageAction::Write(wm) => {
                let mut file = File::create(path)?;
                serde_json::to_writer_pretty(&mut file, &wm)?;
            }
        }
        Ok(())
    }
}

pub fn append_storage_subpath(
    dst: &mut PathBuf,
    source_url: impl AsRef<str>,
    target_url: impl AsRef<str>,
) {
    let options = sanitize_filename::Options {
        truncate: true,
        windows: true,
        replacement: "-",
    };

    let hash = sha256::digest(format!("{}|{}", target_url.as_ref(), source_url.as_ref()));

    dst.push(sanitize_with_options(target_url, options.clone()));
    dst.push(sanitize_with_options(source_url, options));
    dst.push(format!("{}.json", hash));
}

pub enum StorageAction {
    Delete {
        source_url: String,
        target_url: String,
    },
    Write(WebmentionRecord),
}

impl StorageAction {
    fn append_storage_subpath(&self, dst: &mut PathBuf) {
        let (source_url, target_url) = match self {
            StorageAction::Delete {
                source_url,
                target_url,
            } => (source_url, target_url),
            StorageAction::Write(wm) => (&wm.source_url, &wm.target_url),
        };

        append_storage_subpath(dst, source_url, target_url);
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::webmention::storage::append_storage_subpath;

    #[test]
    fn append_storage_path_works() {
        let source = "http://bar.spam.com/another/article";
        let target = "http://foo.bar.com/some/article";
        let mut path = PathBuf::new();

        append_storage_subpath(&mut path, source, target);

        assert_eq!(
            path.into_os_string().into_string().unwrap(), 
            "http---foo.bar.com-some-article/http---bar.spam.com-another-article/e10e9d330cb4798e8c6bf194f7ba925e112f0c508ba3607785262c9d754f51de.json",
        );
    }
}
