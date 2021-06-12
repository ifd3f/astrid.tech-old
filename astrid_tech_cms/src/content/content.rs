use vfs::VfsPath;

#[derive(Eq, PartialEq, Debug)]
pub enum ContentType {
    Markdown,
    JupyterNotebook,
    Text,
}

#[derive(Eq, PartialEq, Debug)]
pub struct UnsupportedContentType(String);

impl ContentType {
    pub fn from_ext(ext: &str) -> Result<Self, UnsupportedContentType> {
        match ext {
            "md" | "markdown" => Ok(ContentType::Markdown),
            "ipynb" => Ok(ContentType::JupyterNotebook),
            "txt" => Ok(ContentType::Text),
            _ => Err(UnsupportedContentType(ext.to_string()))
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct PostContent {
    pub content_type: ContentType,
    pub content: String,
}

#[derive(Eq, PartialEq, Debug)]
pub enum FindIndexError {
    NoIndex,
    MultipleIndices(Vec<String>),
}

pub fn find_index(path: &VfsPath) -> Result<String, FindIndexError> {
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

pub enum FileMetaSet {
    EmbeddedMeta { file: VfsPath, content_type: ContentType },
    SeparateMeta { meta: VfsPath, content: VfsPath, content_type: ContentType },
}
