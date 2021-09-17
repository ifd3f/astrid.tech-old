use std::fs::File;
use std::path::{Path, PathBuf};

use super::data::Webmention;
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
) -> Option<Webmention> {
    let mut path = PathBuf::new();
    path.push(wm_dir);
    append_storage_subpath(&mut path, source_url, target_url);

    let file = match File::open(path) {
        Ok(file) => file,
        Err(_) => return None,
    };
    serde_json::from_reader(file).ok()
}

pub enum StorageAction {
    DeleteWebmention {
        source_url: String,
        target_url: String,
    },
    CreateWebmention(Webmention),
    UpdateWebmention(Webmention),
}

impl StorageAction {
    fn append_storage_subpath(&self, dst: &mut PathBuf) {
        let (source_url, target_url) = match self {
            StorageAction::DeleteWebmention {
                source_url,
                target_url,
            } => (source_url, target_url),
            StorageAction::CreateWebmention(wm) => (&wm.source_url, &wm.target_url),
            StorageAction::UpdateWebmention(wm) => (&wm.source_url, &wm.target_url),
        };

        append_storage_subpath(
            dst,
            Url::parse(source_url).unwrap(),
            Url::parse(target_url).unwrap(),
        );
    }

    fn apply(self, path: &Path) {}
}
