use std::borrow::Borrow;
use std::convert::TryFrom;
use std::io::Write;
use std::str::FromStr;

use chrono::{Datelike, DateTime, Utc};
use gray_matter::engine::yaml::YAML;
use gray_matter::entity::ParsedEntityStruct;
use gray_matter::matter::Matter;
use gray_matter::value::pod::Pod;
use serde::{Deserialize, Serialize};
use url::Url;
use uuid::Uuid;
use vfs::{VfsFileType, VfsPath};

use crate::content::content::{ContentType, find_with_name, FindIndexError, PostContent, UnsupportedContentType, ReadPostContentError};
use crate::content::content;
use crate::content::post::Syndication::Scheduled;
use crate::content::post_registry::DateSlug;

#[derive(Debug)]
pub enum PostError {
    Filesystem(vfs::VfsError),
    IO(std::io::Error),
    YAML(serde_yaml::Error),
    Serde(serde_json::error::Error),
    AmbiguousIndex(FindIndexError),
    UnsupportedContentType(UnsupportedContentType),
    ContentTypeDoesNotSupportFrontmatter(ContentType),
    NotADirectory(VfsPath),
    ReadPost(ReadPostContentError),
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

impl From<std::io::Error> for PostError {
    fn from(e: std::io::Error) -> Self {
        PostError::IO(e)
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

impl From<ReadPostContentError> for PostError {
    fn from(e: ReadPostContentError) -> Self {
        PostError::ReadPost(e)
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub struct MediaEntry {
    image: String,
    caption: String,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub struct RecipeStep {
    text: String
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
pub enum SyndicationStrategy {
    TitleOnly,
    ContentOnly,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(tag = "status", rename_all = "camelCase")]
pub enum Syndication {
    Scheduled {
        url: Url,
        strategy: Option<SyndicationStrategy>,
    },
    Completed {
        url: Url,
        completed_on: DateTime<Utc>,
    },
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(tag = "type")]
pub enum HType {
    #[serde(rename = "entry")]
    Entry,
    #[serde(rename = "recipe")]
    Recipe {
        //duration: Option<Duration>,
        ingredients: Vec<String>,
        instructions: Vec<RecipeStep>,
    },
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EmbeddedMeta {
    pub title: Option<String>,
    pub description: Option<String>,
    pub short_name: Option<String>,
    pub uuid: Uuid,

    pub date: DateTime<Utc>,
    pub published_date: Option<DateTime<Utc>>,
    pub updated_date: Option<DateTime<Utc>>,
    #[serde(default)]
    pub ordinal: usize,

    pub reply_to: Option<Url>,
    pub repost_of: Option<Url>,
    #[serde(default)]
    pub tags: Vec<String>,
    #[serde(default)]
    pub syndications: Vec<Syndication>,
    #[serde(flatten)]
    pub h_type: HType,
    #[serde(default)]
    pub media: Vec<MediaEntry>,
}

impl EmbeddedMeta {
    pub fn get_slug(&self) -> DateSlug {
        DateSlug {
            year: self.date.year(),
            month: self.date.month() as u8,
            day: self.date.day() as u8,
            ordinal: self.ordinal,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct BarePost {
    pub meta: EmbeddedMeta,
    pub content: PostContent,
}

impl BarePost {
    fn write_to(&self, path: &mut VfsPath) -> Result<(), PostError> {
        // TODO
        if self.content.content_type.supports_frontmatter() {
            // If our content type supports having frontmatter, create a single file.
            let mut file = path.join("index.md")?.create_file()?;
            let meta_yaml = serde_yaml::to_string(&self.meta)?;
            file.write(meta_yaml.as_bytes());
            file.write("\n---\n".as_ref());
            file.write(self.content.content.as_bytes());
            return Ok(());
        }

        {
            let mut meta_file = path.create_file()?;
            serde_yaml::to_writer(meta_file, &self.meta);
        }
        {
            let mut content_file = path.join("index.yml")?.create_file()?;
            content_file.write(self.content.content.as_bytes());
        }

        Ok(())
    }

    pub fn get_slug(&self) -> DateSlug {
        self.meta.get_slug()
    }
}


impl TryFrom<VfsPath> for BarePost {
    type Error = PostError;

    /// Creates a post from a post directory.
    fn try_from(path: VfsPath) -> Result<Self, Self::Error> {
        if path.metadata()?.file_type != VfsFileType::Directory {
            Err(PostError::NotADirectory(path.clone()))?;
        }

        let index_name = find_with_name("index", &path)?;
        let index_path = path.join(index_name.as_str())?;

        let ext = index_path.extension().unwrap();
        if ext == "yaml" || ext == "yml" {
            let file = index_path.open_file()?;
            let meta: EmbeddedMeta = serde_yaml::from_reader(file)?;

            let content_filename = find_with_name("content", &path)?;
            let content_path = path.join(content_filename.as_str())?;
            let content = PostContent::try_from(content_path)?;

            return Ok(BarePost { meta, content });
        }

        let content_type = ContentType::from_ext(ext.as_str())?;
        if !content_type.supports_frontmatter() {
            Err(PostError::ContentTypeDoesNotSupportFrontmatter(content_type))?
        } else {
            let contents = index_path.read_to_string()?;
            let matter = Matter::<YAML>::new();
            let parsed: ParsedEntityStruct<EmbeddedMeta> = matter.matter_struct(contents);

            let content = PostContent::new(content_type, parsed.content);
            Ok(BarePost { meta: parsed.data, content })
        }
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryFrom;

    use vfs::{MemoryFS, VfsPath};

    use crate::content::content::ContentType;
    use crate::content::post::{BarePost, EmbeddedMeta, HType, YAMLPostSchema};

    const TXT_ARTICLE_YAML: &str = r#"
        date: 2021-06-12 10:51:30 +08:00
        title: Example post with txt

        type: entry
        shortName: foo-bar
        uuid: 2fdb77e7-a019-4e51-9ba4-cd6b2eedd60e
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
        let parsed: YAMLPostSchema = serde_yaml::from_str(TXT_ARTICLE_YAML).unwrap();

        assert_eq!(parsed.meta.h_type, HType::Entry);
    }

    #[test]
    fn reads_article() {
        let path = setup_working_separate_meta_post();
        let post_path = path.join("index.yaml").unwrap();

        let post = BarePost::try_from(post_path).unwrap();

        assert_eq!(post.content.content_type, ContentType::Text);
        assert_eq!(post.content.content, TXT_CONTENTS);
    }
}
