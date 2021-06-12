use std::borrow::Borrow;
use std::convert::TryFrom;
use std::str::FromStr;

use chrono::{DateTime, Utc};
use gray_matter::engine::yaml::YAML;
use gray_matter::matter::Matter;
use gray_matter::value::pod::Pod;
use serde::{Deserialize, Serialize};
use vfs::{VfsFileType, VfsPath};

use crate::content::content::{ContentType, FindIndexError, PostContent, UnsupportedContentType};
use crate::content::content;

#[derive(Eq, PartialEq, Debug)]
pub struct Post {
    name: String,
    content: PostContent,
    meta: EmbeddedMeta,
}

impl Post {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

#[derive(Debug)]
pub enum PostError {
    Filesystem(vfs::VfsError),
    YAML(serde_yaml::Error),
    Serde(serde_json::error::Error),
    AmbiguousIndex(FindIndexError),
    UnsupportedContentType(UnsupportedContentType),
}

impl From<vfs::VfsError> for PostError {
    fn from(e: vfs::VfsError) -> Self {
        PostError::Filesystem(e)
    }
}

impl From<serde_yaml::Error> for PostError {
    fn from(e: serde_yaml::Error) -> Self {
        PostError::YAML(e)
    }
}

impl From<serde_json::Error> for PostError {
    fn from(e: serde_json::Error) -> Self {
        PostError::Serde(e)
    }
}

impl From<UnsupportedContentType> for PostError {
    fn from(e: UnsupportedContentType) -> Self {
        PostError::UnsupportedContentType(e)
    }
}

impl From<FindIndexError> for PostError {
    fn from(e: FindIndexError) -> Self {
        PostError::AmbiguousIndex(e)
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
struct MediaEntry {
    image: String,
    caption: String,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
struct RecipeStep {
    text: String
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(tag = "type")]
enum PostType {
    #[serde(rename = "entry")]
    Entry,
    #[serde(rename = "recipe")]
    Recipe {
        //duration: Option<Duration>,
        ingredients: Vec<String>,
        instructions: Vec<RecipeStep>
    },
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
struct EmbeddedMeta {
    title: Option<String>,
    description: Option<String>,
    date: DateTime<Utc>,
    published_date: Option<DateTime<Utc>>,
    short_name: Option<String>,
    #[serde(default)]
    ordinal: usize,
    #[serde(flatten)]
    post_type: PostType,
    #[serde(default)]
    tags: Vec<String>,
    #[serde(default)]
    media: Vec<MediaEntry>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct SeparateYAMLMeta {
    content_path: String,
    #[serde(flatten)]
    meta: EmbeddedMeta,
}

impl TryFrom<&VfsPath> for Post {
    type Error = PostError;

    fn try_from(path: &VfsPath) -> Result<Self, Self::Error> {
        let (name, meta, content) = match path.metadata()?.file_type {
            VfsFileType::File => {
                todo!()
            }
            VfsFileType::Directory => {
                // Search for index file
                let index = content::find_index(path)?;
                let index_file = path.join(index.as_str())?;

                let (meta, content) = match index.as_str() {
                    "index.yaml" => {
                        let file = index_file.open_file()?;
                        let meta: SeparateYAMLMeta = serde_yaml::from_reader(file)?;
                        let content_file = path.join(meta.content_path.as_str())?;
                        let content_type = ContentType::from_ext(content_file.extension().unwrap().as_str())?;
                        (meta.meta, PostContent { content: content_file.read_to_string()?, content_type })
                    }
                    "index.md" => {
                        let contents = {
                            let mut string = String::new();
                            index_file.open_file()?.read_to_string(&mut string);
                            string
                        };
                        let matter = Matter::<YAML>::new();
                        let parsed = matter.matter(contents);
                        let meta: EmbeddedMeta = parsed.data.deserialize()?;
                        (meta, PostContent { content: parsed.content, content_type: ContentType::Markdown })
                    }
                    _ => {
                        todo!()
                    }
                };

                (path.filename(), meta, content)
            }
        };

        Ok(Post {
            name,
            content,
            meta,
        })
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryFrom;

    use vfs::{MemoryFS, VfsPath};

    use crate::content::content::ContentType;
    use crate::content::post::{EmbeddedMeta, Post, PostType, SeparateYAMLMeta};

    const TXT_ARTICLE_YAML: &str = r#"
        date: 2021-06-12 10:51:30 +08:00
        title: Example post with txt

        type: entry
        shortName: foo-bar
        ordinal: 0
        contentPath: "post.txt"
        tags:
          - rust
          - python
          - csharp
        "#;
    const TXT_CONTENTS: &str = r#"
        foo bar spam
        "#;

    fn setup_working_separate_meta_post() -> VfsPath {
        let fs = MemoryFS::new();
        let root = VfsPath::new(fs);

        let mut file = root.join("index.yaml").unwrap().create_file().unwrap();
        file.write(TXT_ARTICLE_YAML.as_ref());

        let mut file = root.join("post.txt").unwrap().create_file().unwrap();
        file.write(TXT_CONTENTS.as_ref());
        root
    }

    #[test]
    fn parses_article_meta() {
        let parsed: SeparateYAMLMeta = serde_yaml::from_str(TXT_ARTICLE_YAML).unwrap();

        assert_eq!(parsed.meta.post_type, PostType::Entry);
    }

    #[test]
    fn reads_article() {
        let path = setup_working_separate_meta_post();

        let post = Post::try_from(&path).unwrap();

        assert_eq!(post.content.content_type, ContentType::Text);
        assert_eq!(post.content.content, TXT_CONTENTS);
    }
}
