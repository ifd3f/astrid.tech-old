use std::fs::File;
use std::io::Read;
use std::path::Path;

use pandoc::PandocOption;

use at_objects::input_types::ArticleMeta;

use crate::document::DocumentLoadError;

pub struct Article {}

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
    pub fn read(path: &str) {}
}

#[cfg(test)]
mod test {
    use crate::test_util::get_resources_path;

    #[test]
    fn read_article() {
        let path = get_resources_path("blog-posts/site-release.md");
    }
}