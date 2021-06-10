use std::convert::TryFrom;

use serde::{Deserialize, Serialize};
use serde_yaml::Error;
use vfs::{VfsFileType, VfsPath};

pub struct Post {
    name: String,
    file_contents: String,
}

impl Post {
    fn write_to(&self, path: &mut VfsPath) {
        todo!()
    }
}

enum PostError {
    Filesystem(vfs::VfsError),
    YAML(serde_yaml::Error),
    AmbiguousIndex(FindIndexError),
}

impl From<vfs::VfsError> for PostError {
    fn from(e: vfs::VfsError) -> Self {
        PostError::Filesystem(e)
    }
}

impl From<serde_yaml::Error> for PostError {
    fn from(e: Error) -> Self {
        PostError::YAML(e)
    }
}

impl From<FindIndexError> for PostError {
    fn from(e: FindIndexError) -> Self {
        PostError::AmbiguousIndex(e)
    }
}

#[derive(Serialize, Deserialize)]
enum PostType {
    #[serde(rename = "post")]
    Post,
    #[serde(rename = "note")]
    Note,
    #[serde(rename = "recipe")]
    Recipe,
    #[serde(rename = "images")]
    Images,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct StoredMeta {
    content_type: PostType,
    content: Option<String>,
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

                match index.as_str() {
                    "index.yaml" => {
                        let file = index_file.open_file()?;
                        let meta = serde_yaml::from_reader(file)? as StoredMeta;
                    }
                    "index.md" => {}
                    _ => {}
                }
                if let Some(path) = meta {
                    let file = path.open_file()?;
                    let meta = serde_yaml::from_reader(file)? as StoredMeta;
                    let content_path = match &meta.content {
                        None => find_index(&path)?.as_str(),
                        Some(content) => content.as_str()
                    };

                    let data = path.join(content_path)?.read_to_string()?;
                    (path.filename(), meta, data)
                } else {
                    (todo!(), todo!(), todo!())
                }
            }
        };

        Ok(Post {
            name,
            file_contents,
        })
    }
}
