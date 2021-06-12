use vfs::VfsPath;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ContentType {
    Markdown,
    ReStructuredText,
    JupyterNotebook,
    Text,
    HTMLFragment,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct UnsupportedContentType(String);

impl ContentType {
    pub fn from_ext(ext: &str) -> Result<Self, UnsupportedContentType> {
        Ok(match ext {
            "md" | "markdown" => ContentType::Markdown,
            "inc" => ContentType::HTMLFragment,
            "ipynb" => ContentType::JupyterNotebook,
            "txt" => ContentType::Text,
            "rst" => ContentType::ReStructuredText,
            _ => Err(UnsupportedContentType(ext.to_string()))?
        })
    }

    pub fn to_ext(&self) -> &'static str {
        match self {
            ContentType::Markdown => "md",
            ContentType::ReStructuredText => "rst",
            ContentType::JupyterNotebook => "ipynb",
            ContentType::Text => "txt",
            ContentType::HTMLFragment => "inc"
        }
    }

    pub fn from_mimetype(mimetype: &str) -> Result<Self, UnsupportedContentType> {
        Ok(match mimetype {
            "text/markdown" => ContentType::Markdown,
            "text/x-rst" => ContentType::ReStructuredText,
            "application/x-ipynb+json" => ContentType::JupyterNotebook,
            "text/plain" => ContentType::Text,
            "text/html" => ContentType::HTMLFragment,
            _ => Err(UnsupportedContentType(mimetype.to_string()))?
        })
    }

    pub fn to_mimetype(&self) -> &'static str {
        match self {
            ContentType::Markdown => "text/markdown",
            ContentType::ReStructuredText => "text/x-rst",
            ContentType::JupyterNotebook => "application/x-ipynb+json",
            ContentType::Text => "text/plain",
            ContentType::HTMLFragment => "text/html"
        }
    }

    pub fn supports_frontmatter(&self) -> bool {
        match self {
            ContentType::ReStructuredText => true,
            ContentType::Markdown => true,
            ContentType::JupyterNotebook => false,
            ContentType::Text => true,
            ContentType::HTMLFragment => false,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct PostContent {
    pub content_type: ContentType,
    pub content_path: String,
    pub content: String,
}

#[derive(Eq, PartialEq, Debug, Clone)]
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
