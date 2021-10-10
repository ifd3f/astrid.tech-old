use std::{
    ffi::OsStr,
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

enum ContentSourceType<'a> {
    Embedded(ContentType, &'a str),
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

struct TransformedContent {
    html: String,
    assets: Vec<String>, // TODO
}

type ReadDocumentResult<T> = Result<T, NonDocument>;
type ContentResult<T> = Result<T, String>;

fn read_document<T: DeserializeOwned>(
    extension: &OsStr,
    file_content: &str,
) -> ReadDocumentResult<DocumentWithPossiblyEmbeddedContent<T>> {
    match extension.to_string_lossy().as_ref() {
        "md" | "markdown" => {
            let entity = Matter::<YAML>::new().parse(file_content);
            let frontmatter: Option<Pod> = entity.data;

            if let Some(document) = frontmatter {
                Ok(DocumentWithPossiblyEmbeddedContent {
                    document: document.deserialize()?,
                    embedded_content: Some(entity.content),
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
}

fn extract_content_source(
    path: &Path,
    content_info: DocumentWithPossiblyEmbeddedContent<Option<file_schema::Content>>,
) -> ContentResult<ContentSourceType> {
    if let Some(embedded_content) = content_info.embedded_content {
        return if let Some(_) = content_info.document {
            Err("Documents with embedded content cannot reference other sources".to_string())
        } else {
            Ok(ContentSourceType::Embedded(
                embedded_content.content_type,
                &embedded_content.raw,
            ))
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
        Some(file_schema::Content::EmbeddedPlaintext(plaintext)) => Ok(
            ContentSourceType::Embedded(ContentType::Plaintext, &plaintext),
        ),
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
        ContentSourceType::Embedded(content_type, raw) => Content {
            content_type,
            raw: raw.to_string(),
        },
        ContentSourceType::FileRef(path) => match path.extension() {
            Some(ext) => match ext.to_string_lossy().as_ref() {
                "html" | "htm" => Content {
                    content_type: ContentType::Html,
                    raw: fs::read_to_string(path)?,
                },
                "md" | "markdown" => Content {
                    content_type: ContentType::Markdown,
                    raw: fs::read_to_string(path)?,
                },
                "txt" => Content {
                    content_type: ContentType::Plaintext,
                    raw: fs::read_to_string(path)?,
                },
                _ => Err(format!(
                    "Unsupported content at path {}",
                    path.to_string_lossy()
                ))?,
            },
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
