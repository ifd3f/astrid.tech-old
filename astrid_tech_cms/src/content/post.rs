use std::convert::TryFrom;

use chrono::{DateTime, Utc};
use gray_matter::engine::yaml::YAML;
use gray_matter::matter::Matter;
use gray_matter::value::pod::Pod;
use serde::{Deserialize, Serialize};
use vfs::{VfsFileType, VfsPath};
use std::borrow::Borrow;

pub struct Post {
    name: String,
    file_contents: String,
    meta: EmbeddedMeta
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

impl From<FindIndexError> for PostError {
    fn from(e: FindIndexError) -> Self {
        PostError::AmbiguousIndex(e)
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct ImageEntry {
    image: String,
    caption: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "type", rename_all = "camelCase")]
enum PostType {
    #[serde(rename = "article")]
    Article {
        title: String,
        description: Option<String>,
    },
    #[serde(rename = "note")]
    Note,
    #[serde(rename = "recipe")]
    Recipe {
        title: String,
        description: Option<String>,
    },
    #[serde(rename = "image")]
    Image {
        title: String,
        description: Option<String>,
        images: Vec<ImageEntry>,
    },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct EmbeddedMeta {
    date: DateTime<Utc>,
    #[serde(flatten)]
    post_type: PostType,
    tags: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
struct SeparateYAMLMeta {
    content_path: String,
    #[serde(flatten)]
    meta: EmbeddedMeta,
}

#[derive(Eq, PartialEq, Debug)]
enum FindIndexError {
    NoIndex,
    MultipleIndices(Vec<String>),
}

fn find_index(path: &VfsPath) -> Result<String, FindIndexError> {
    let mut indices: Vec<String> = path.read_dir().unwrap()
        .filter_map(|c| {
            let name = c.filename();
            if name.starts_with("index.") {
                Some(name)
            } else {
                None
            }
        })
        .collect();

    match indices.len() {
        0 => Err(FindIndexError::NoIndex),
        1 => Ok(indices[0].clone()),
        _ => Err(FindIndexError::MultipleIndices(indices))
    }
}

impl TryFrom<&VfsPath> for Post {
    type Error = PostError;

    fn try_from(path: &VfsPath) -> Result<Self, Self::Error> {
        let (name, meta, file_contents) = match path.metadata()?.file_type {
            VfsFileType::File => {
                todo!()
            }
            VfsFileType::Directory => {
                // Search for index file
                let index = find_index(path)?;
                let index_file = path.join(index.as_str())?;

                let (meta, content) = match index.as_str() {
                    "index.yaml" => {
                        let file = index_file.open_file()?;
                        let meta: SeparateYAMLMeta = serde_yaml::from_reader(file)?;
                        let content_file = path.join(meta.content_path.as_str())?;
                        (meta.meta, content_file.read_to_string()?)
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
                        (meta, parsed.content)
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
            file_contents,
            meta
        })
    }
}

#[cfg(test)]
mod test {
    use crate::content::post::{Post, EmbeddedMeta, PostType, SeparateYAMLMeta};
    use vfs::{MemoryFS, VfsPath};
    use std::convert::TryFrom;

    const TXT_ARTICLE_YAML: &str = r#"
        date: 2021-06-12 10:51:30 +08:00
        title: Example post with txt

        type: article
        content_path: "post.txt"
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

        if let PostType::Article { title, description } = parsed.meta.post_type {
            assert_eq!(title, "Example post with txt");
            assert_eq!(description, None);
        } else {
            panic!("Post is not an Article")
        }
    }

    #[test]
    fn reads_article() {
        let path = setup_working_separate_meta_post();

        let post = Post::try_from(&path).unwrap();

        assert_eq!(post.file_contents, TXT_CONTENTS)
    }
}
