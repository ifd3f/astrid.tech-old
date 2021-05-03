use std::path::Path;
use std::fs::File;
use std::io::Read;
use crate::markdown::{load_markdown, MarkdownLoadError};
use at_objects::input_types::ArticleMeta;
use pandoc::PandocOption;

pub struct Article {

}

pub enum ArticleReadError {
    Markdown(MarkdownLoadError),
    NoMeta
}

impl From<MarkdownLoadError> for ArticleReadError {
    fn from(e: MarkdownLoadError) -> Self {
        MarkdownLoadError(e)
    }
}

impl Article {
    pub fn read(path: &str) {
        let mut pandoc = pandoc::new();
        pandoc.add_option(PandocOption::O)
    }
}

#[cfg(test)]
mod test {
    use at_common::test_util::get_resources_path;

    #[test]
    fn read_article() {
        let path = get_resources_path("blog-posts/site-release.md");

    }
}