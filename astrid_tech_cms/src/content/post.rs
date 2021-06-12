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
use vfs::{VfsFileType, VfsPath};

use crate::content::content::{ContentType, FindIndexError, PostContent, UnsupportedContentType};
use crate::content::content;
use crate::content::post_registry::DateSlug;
use url::Url;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct BarePost {
    pub content: PostContent,
    pub meta: EmbeddedMeta,
}

impl BarePost {
    fn write_to(&self, path: &mut VfsPath) -> Result<(), PostError> {
        // TODO
        if self.content.content_type.supports_frontmatter() {
            // If our content type supports having frontmatter, create a single file.
            let mut file = path.create_file()?;
            let meta_yaml = serde_yaml::to_string(&self.meta)?;
            file.write(meta_yaml.as_bytes());
            file.write("\n---\n".as_ref());
            file.write(self.content.content.as_bytes());
        } else {
            // If our content type does not support having frontmatter, we must use the YAML format.
            // First, identify if we need to have.
            let content = YAMLContent::from(&self.content);

            if let YAMLContent::Separate { content_path } = &content {
                let content_path = path.parent().unwrap()
                    .join(content_path.as_str())?;
                content_path.create_dir_all()?;

                let mut file = content_path.create_file()?;
                file.write(self.content.content.as_bytes());
            }

            {
                let mut meta_file = path.create_file()?;
                let data = YAMLPostSchema {
                    content,
                    meta: self.meta.clone(),
                };
                serde_yaml::to_writer(meta_file, &data);
            }
        }
        Ok(())
    }

    pub fn get_slug(&self) -> DateSlug {
        self.meta.get_slug()
    }
}

#[derive(Debug)]
pub enum PostError {
    Filesystem(vfs::VfsError),
    IO(std::io::Error),
    YAML(serde_yaml::Error),
    Serde(serde_json::error::Error),
    AmbiguousIndex(FindIndexError),
    UnsupportedContentType(UnsupportedContentType),
    ContentTypeDoesNotSupportFrontmatter(ContentType),
    NotAFile(VfsPath),
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

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
struct MediaEntry {
    image: String,
    caption: String,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
struct RecipeStep {
    text: String
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
enum SyndicationStrategy {
    TitleOnly,
    ContentOnly,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(tag = "status", rename_all = "camelCase")]
enum Syndication {
    Scheduled {
        url: Url,
        strategy: Option<SyndicationStrategy>,
    },
    Attempting {
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
enum HType {
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
struct EmbeddedMeta {
    title: Option<String>,
    description: Option<String>,
    short_name: Option<String>,

    date: DateTime<Utc>,
    published_date: Option<DateTime<Utc>>,
    updated_date: Option<DateTime<Utc>>,
    #[serde(default)]
    ordinal: usize,

    reply_to: Option<Url>,
    #[serde(default)]
    tags: Vec<String>,
    #[serde(default)]
    syndications: Vec<Syndication>,
    #[serde(flatten)]
    h_type: HType,
    #[serde(default)]
    media: Vec<MediaEntry>,
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


#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum YAMLContent {
    #[serde(rename_all = "camelCase")]
    Separate { content_path: String },
    #[serde(rename_all = "camelCase")]
    Embedded { content: String, content_type: String },
}

impl From<&PostContent> for YAMLContent {
    fn from(content: &PostContent) -> Self {
        if content.content_path == "." {
            YAMLContent::Embedded {
                content: content.content.clone(),
                content_type: content.content_type.to_mimetype().to_string(),
            }
        } else {
            YAMLContent::Separate {
                content_path: content.content_path.clone()
            }
        }
    }
}

impl YAMLContent {
    fn into_content(self, dir: VfsPath) -> Result<PostContent, PostError> {
        Ok(match self {
            YAMLContent::Separate { content_path } => {
                let content_file = dir.join(content_path.as_str())?;
                let content_type = ContentType::from_ext(content_file.extension().unwrap().as_str())?;
                PostContent {
                    content: content_file.read_to_string()?,
                    content_path,
                    content_type,
                }
            }
            YAMLContent::Embedded { content, content_type } => {
                PostContent {
                    content,
                    content_path: ".".to_string(),
                    content_type: ContentType::from_mimetype(content_type.as_str())?,
                }
            }
        })
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct YAMLPostSchema {
    #[serde(flatten)]
    content: YAMLContent,
    #[serde(flatten)]
    meta: EmbeddedMeta,
}

impl TryFrom<VfsPath> for BarePost {
    type Error = PostError;

    /// Creates a post from a post file.
    fn try_from(path: VfsPath) -> Result<Self, Self::Error> {
        if path.metadata()?.file_type != VfsFileType::File {
            Err(PostError::NotAFile(path.clone()))?;
        }

        let ext = path.extension().unwrap();
        let (meta, content) = if ext == "yaml" || ext == "yml" {
            let file = path.open_file()?;
            let meta: YAMLPostSchema = serde_yaml::from_reader(file)?;
            (meta.meta, meta.content.into_content(path.parent().unwrap())?)
        } else {
            let content_type = ContentType::from_ext(ext.as_str())?;
            if !content_type.supports_frontmatter() {
                Err(PostError::ContentTypeDoesNotSupportFrontmatter(content_type))?
            } else {
                let contents = {
                    let mut string = String::new();
                    path.open_file()?.read_to_string(&mut string)?;
                    string
                };
                let matter = Matter::<YAML>::new();
                let parsed: ParsedEntityStruct<EmbeddedMeta> = matter.matter_struct(contents);
                (parsed.data, PostContent { content: parsed.content, content_type, content_path: ".".to_string() })
            }
        };

        Ok(BarePost {
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
    use crate::content::post::{BarePost, EmbeddedMeta, HType, YAMLPostSchema};

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
