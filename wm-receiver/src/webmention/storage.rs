use std::error::Error;
use std::fs::{self, File};
use std::path::{Path, PathBuf};

use super::data::WebmentionRecord;
use slug::slugify;
use url::Url;

pub fn append_storage_subpath(dst: &mut PathBuf, source_url: Url, target_url: Url) {
    dst.push(target_url.host().unwrap().to_string());
    dst.push(slugify(target_url.path()));
    dst.push(source_url.host().unwrap().to_string());
    dst.push(format!("{}.yml", slugify(source_url.path())));
}

pub fn read_existing_webmention(
    wm_dir: impl AsRef<Path>,
    source_url: Url,
    target_url: Url,
) -> Option<WebmentionRecord> {
    let mut path = PathBuf::new();
    path.push(wm_dir);
    append_storage_subpath(&mut path, source_url, target_url);

    let file = match File::open(path) {
        Ok(file) => file,
        Err(_) => return None,
    };
    serde_yaml::from_reader(file).ok()
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

        append_storage_subpath(
            dst,
            Url::parse(source_url).unwrap(),
            Url::parse(target_url).unwrap(),
        );
    }

    pub fn apply(self, wm_dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
        let mut path = PathBuf::new();
        path.push(wm_dir);
        self.append_storage_subpath(&mut path);

        // Safe to unwrap because we've created subdirectories
        let parent = path.parent().unwrap();
        fs::create_dir_all(&parent)?;

        match self {
            StorageAction::Delete { .. } => {
                fs::remove_file(path)?;
            }
            StorageAction::Write(wm) => {
                let mut file = File::create(path)?;
                serde_yaml::to_writer(&mut file, &wm)?;
            }
        }
        Ok(())
    }
}
