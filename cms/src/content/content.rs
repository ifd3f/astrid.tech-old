use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::__private::{Formatter, TryFrom};
use serde::de::{Error, Visitor};
use vfs::{VfsError, VfsPath};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ContentType {
    Markdown,
    ReStructuredText,
    JupyterNotebook,
    Text,
    HTMLFragment,
}

impl Serialize for ContentType {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error> where
        S: Serializer {
        serializer.serialize_str(self.to_mimetype())
    }
}

struct ContentTypeVisitor;

impl<'de> Visitor<'de> for ContentTypeVisitor {
    type Value = ContentType;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a mimetype")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: Error,
    {
        ContentType::from_mimetype(v)
            .map_err(|e| E::custom(format!("Unsupported mimetype {}", e.0)))
    }
}

impl<'de> Deserialize<'de> for ContentType {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        deserializer.deserialize_str(ContentTypeVisitor)
    }
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
pub struct Content {
    pub(crate) content_type: ContentType,
    pub(crate) content: String,
}

#[derive(Debug)]
pub enum ReadPostContentError {
    NoExtension,
    Filesystem(VfsError),
    InvalidExtension(UnsupportedContentType),
}

impl From<VfsError> for ReadPostContentError {
    fn from(e: VfsError) -> Self {
        ReadPostContentError::Filesystem(e)
    }
}

impl From<UnsupportedContentType> for ReadPostContentError {
    fn from(e: UnsupportedContentType) -> Self {
        ReadPostContentError::InvalidExtension(e)
    }
}

impl TryFrom<VfsPath> for Content {
    type Error = ReadPostContentError;

    fn try_from(path: VfsPath) -> Result<Self, Self::Error> {
        let ext = path.extension().ok_or(ReadPostContentError::NoExtension)?;
        let content_type = ContentType::from_ext(ext.as_str())?;
        let content = path.read_to_string()?;
        Ok(Content {
            content_type,
            content,
        })
    }
}

impl Content {
    pub fn new(content_type: ContentType, content: String) -> Content {
        Content { content_type, content }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum FindFilenameError {
    NotFound,
    Multiple(Vec<String>),
}

pub fn find_unique_with_name(name: &str, path: &VfsPath) -> Result<String, FindFilenameError> {
    let mut prefix = name.to_string();
    prefix.push('.');

    let indices: Vec<String> = path.read_dir().unwrap()
        .filter_map(|c| {
            let name = c.filename();
            if name.starts_with(prefix.as_str()) {
                Some(name)
            } else {
                None
            }
        })
        .collect();

    match indices.len() {
        0 => Err(FindFilenameError::NotFound),
        1 => Ok(indices[0].clone()),
        _ => Err(FindFilenameError::Multiple(indices))
    }
}
