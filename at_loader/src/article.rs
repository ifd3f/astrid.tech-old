use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use pandoc::PandocOption;

use at_objects::input_types::ArticleMeta;

use crate::document::{DocumentLoadError, Document};

pub struct Article {
    meta: ArticleMeta,
    rawContent: String,
    assetDir: PathBuf
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
    pub fn read(path: &Path) {
        let document = Document::<ArticleMeta>::load_path(path)?;
        let meta = document.meta.ok_or(ArticleReadError::NoMeta)?;


    }
}

#[cfg(test)]
mod test {
    use crate::test_util::get_resources_path;

    #[test]
    fn read_article() {
        let path = get_resources_path("blog-posts/site-release.md");
    }
}