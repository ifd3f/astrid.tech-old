use std::{fs::File, io::Read, path::{Path, PathBuf}};

use url::Url;
use slug::slugify;
use super::data::Webmention;

pub fn append_storage_subpath(dst: &mut PathBuf, source_url: Url, target_url: Url) {
    dst.push(target_url.host().unwrap().to_string());
    dst.push(slugify(target_url.path()));
    dst.push(source_url.host().unwrap().to_string());
    dst.push(format!("{}.yml", slugify(source_url.path())));
}

pub fn read_existing_webmention<'a>(
    buf: &'a mut String,
    wm_dir: impl AsRef<Path>,
    source_url: Url,
    target_url: Url,
) -> Option<Webmention<'a>> {
    let mut path = PathBuf::new();
    path.push(wm_dir);
    append_storage_subpath(&mut path, source_url, target_url);

    let file = match File::open(path) {
        Ok(file) =>file,
        Err(_) => return None,
    };
    file.read_to_string(buf);
    serde_json::from_str(buf.as_str()).ok()
}

pub enum StorageAction<'a> {
    DeleteWebmention {
        source_url: &'a str,
        target_url: &'a str,
    },
    CreateWebmention(Webmention<'a>),
    UpdateWebmention(Webmention<'a>),
}

impl<'a> StorageAction<'a> {
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
