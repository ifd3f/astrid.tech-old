use std::path::{Path, PathBuf};

use crate::document::DocumentLoadError;
use crate::input_types::ArticleMeta;
use crate::page::Page;
use crate::timestream::{TimestreamObject, DatedSlug};
use chrono::Datelike;


pub struct Article {
    meta: ArticleMeta,
    raw_content: String,
    asset_dir: PathBuf,
    page: Page,
}

impl TimestreamObject for Article {
    fn get_slug(&self, year: i32, month: u32, day: u32, ordinal: u32) -> Option<DatedSlug> {
        let date = &self.meta.date.date();

        if (date.year(), date.month(), date.day()) == (year, month, day) {
            Some(DatedSlug {
                year,
                month,
                day,
                ordinal,
                short_name: self.page.short_name.to_string(),
            })
        } else {
            None
        }
    }

    fn get_preview(&self) {
        unimplemented!()
    }

    fn get_page(&self) {
        unimplemented!()
    }
}

pub enum ArticleReadError {
    DocumentLoad(DocumentLoadError),
    NoMeta,
}

impl From<DocumentLoadError> for ArticleReadError {
    fn from(e: DocumentLoadError) -> Self {
        ArticleReadError::DocumentLoad(e)
    }
}

impl Article {
    //pub fn read(path: &Path) {
    //    let document = Document::<ArticleMeta>::load_path(path)?;
    //    let meta = document.meta.ok_or(ArticleReadError::NoMeta)?;
    //}
}

#[cfg(test)]
mod test {
    use crate::test_util::get_resources_path;

    #[test]
    fn read_article() {
        let path = get_resources_path("blog-posts/site-release.md");
    }
}