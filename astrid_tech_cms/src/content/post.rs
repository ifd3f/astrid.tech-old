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
}

impl Post {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

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

#[derive(Serialize, Deserialize)]
struct ImageEntry {
    image: String,
    caption: String
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
enum PostType {
    #[serde(rename = "post")]
    Article {
        title: String,
        description: String,
    },
    #[serde(rename = "note")]
    Note,
    #[serde(rename = "recipe")]
    Recipe {
        title: String,
        description: String
    },
    #[serde(rename = "images")]
    Images {
        title: String,
        description: String,
        images: Vec<ImageEntry>
    },
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct StoredMeta {
    date: DateTime<Utc>,
    #[serde(flatten)]
    post_type: PostType,
    content_path: Option<String>,
    tags: Vec<String>,
}

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
                // Search for index file, and verify integrity
                let index = find_index(path)?;
                let index_file = path.join(index.as_str())?;

                let (meta, content) = match index.as_str() {
                    "index.yaml" => {
                        let file = index_file.open_file()?;
                        let meta: StoredMeta = serde_yaml::from_reader(file)?;
                        let path = meta.content_path.clone().unwrap();
                        let content_file = index_file.join(path.as_str())?;
                        (meta, content_file.read_to_string()?)
                    }
                    "index.md" => {
                        let contents = {
                            let mut string = String::new();
                            index_file.open_file()?.read_to_string(&mut string);
                            string
                        };
                        let matter = Matter::<YAML>::new();
                        let parsed = matter.matter(contents);
                        let meta: StoredMeta = parsed.data.deserialize()?;
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
        })
    }
}
