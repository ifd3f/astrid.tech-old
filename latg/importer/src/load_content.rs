use std::{
    ffi::OsStr,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

use gray_matter::{engine::YAML, Matter, Pod};
use serde::{de::DeserializeOwned, Deserialize};

use crate::file_schema::{self, Document};

struct Content {
    content_type: ContentType,
    raw: String,
}

struct DocumentWithPossiblyEmbeddedContent<T> {
    embedded_content: Option<Content>,
    document: T,
}

enum ContentSourceType {
    Embedded(Content),
    FileRef(PathBuf),
}

enum ContentType {
    Plaintext,
    Markdown,
    Html,
    // Jupyter,
}

enum NonDocument {
    Invalid(String),
    NonDocument,
}

pub struct TransformedContent {
    html: String,
    assets: Vec<String>, // TODO
}

pub type ReadDocumentResult<T> = Result<T, NonDocument>;
type ContentResult<T> = Result<T, String>;

pub fn load(
    path: impl AsRef<Path>,
) -> ReadDocumentResult<(file_schema::Document, TransformedContent)> {
    let path = path.as_ref();
    let doc: DocumentWithPossiblyEmbeddedContent<file_schema::Document> =
        read_document(path.extension(), &fs::read_to_string(path)?)?;
    let content_source = extract_content_source(
        path,
        DocumentWithPossiblyEmbeddedContent {
            document: &doc.document.content,
            embedded_content: doc.embedded_content,
        },
    )?;
    let loaded = load_content_source(content_source)?;
    let transformed = transform_content(loaded)?;
    Ok((doc.document, transformed))
}

fn read_document<T: DeserializeOwned>(
    extension: Option<&OsStr>,
    file_content: &str,
) -> ReadDocumentResult<DocumentWithPossiblyEmbeddedContent<T>> {
    if let Some(extension) = extension {
        match extension.to_string_lossy().as_ref() {
            "md" | "markdown" => {
                let entity = Matter::<YAML>::new().parse(file_content);
                let frontmatter: Option<Pod> = entity.data;

                if let Some(document) = frontmatter {
                    Ok(DocumentWithPossiblyEmbeddedContent {
                        document: document.deserialize()?,
                        embedded_content: Some(Content {
                            content_type: ContentType::Markdown,
                            raw: entity.content,
                        }),
                    })
                } else {
                    Err(NonDocument::NonDocument)
                }
            }
            "yml" | "yaml" => Ok(DocumentWithPossiblyEmbeddedContent {
                embedded_content: None,
                document: serde_yaml::from_str(file_content)?,
            }),
            "json" => Ok(DocumentWithPossiblyEmbeddedContent {
                embedded_content: None,
                document: serde_json::from_str(file_content)?,
            }),
            "toml" => Ok(DocumentWithPossiblyEmbeddedContent {
                embedded_content: None,
                document: toml::from_str(file_content)?,
            }),
            _ => Err(NonDocument::NonDocument),
        }
    } else {
        Err(NonDocument::NonDocument)
    }
}

fn extract_content_source(
    path: &Path,
    content_info: DocumentWithPossiblyEmbeddedContent<&Option<file_schema::Content>>,
) -> ContentResult<ContentSourceType> {
    if let Some(embedded_content) = content_info.embedded_content {
        return if let Some(_) = content_info.document {
            Err("Documents with embedded content cannot reference other sources".to_string())
        } else {
            Ok(ContentSourceType::Embedded(embedded_content))
        };
    }

    fn ref_without_extension(path: &Path) -> ContentResult<ContentSourceType> {
        if let Some(stem) = path.file_stem() {
            Ok(ContentSourceType::FileRef(PathBuf::from(stem)))
        } else {
            Err(format!(
                "Could not find file name for {}",
                path.to_string_lossy()
            ))
        }
    }

    match content_info.document {
        None => ref_without_extension(path),
        Some(file_schema::Content::EmbeddedPlaintext(plaintext)) => {
            Ok(ContentSourceType::Embedded(Content {
                content_type: ContentType::Plaintext,
                raw: plaintext.to_string(),
            }))
        }
        Some(file_schema::Content::FileRef { src, .. }) => {
            if let Some(src) = src {
                let mut path = PathBuf::from(path);
                path.pop();
                path.push(src);
                Ok(ContentSourceType::FileRef(path))
            } else {
                ref_without_extension(path)
            }
        }
    }
}

fn load_content_source(source_type: ContentSourceType) -> ContentResult<Content> {
    Ok(match source_type {
        ContentSourceType::Embedded(content) => content,
        ContentSourceType::FileRef(path) => match path.extension() {
            Some(ext) => {
                let content_type = match ext.to_string_lossy().as_ref() {
                    "html" | "htm" => ContentType::Html,
                    "md" | "markdown" => ContentType::Markdown,
                    "txt" => ContentType::Plaintext,
                    _ => Err(format!(
                        "Unsupported content at path {}",
                        path.to_string_lossy()
                    ))?,
                };
                let raw = fs::read_to_string(path).map_err(|e| e.to_string())?;
                Content { content_type, raw }
            }
            None => Err(format!("No extension on file {}", path.to_string_lossy()))?,
        },
    })
}

fn transform_content(content: Content) -> ContentResult<TransformedContent> {
    match content.content_type {
        ContentType::Plaintext => Ok(TransformedContent {
            html: format!("<pre>{}</pre>", html_escape::encode_text(&content.raw)),
            assets: vec![],
        }),
        ContentType::Markdown => todo!(),
        ContentType::Html => Ok(TransformedContent {
            html: content.raw,
            assets: vec![],
        }),
    }
}

impl<T: ToString> From<T> for NonDocument {
    fn from(err: T) -> Self {
        NonDocument::Invalid(err.to_string())
    }
}
