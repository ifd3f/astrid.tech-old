use std::path::PathBuf;

use gray_matter::Matter;
use serde::Deserialize;

use crate::file_schema::{self, Document};

struct Content {
    content_type: ContentType,
    raw: String,
}

struct DocumentWithPossiblyEmbeddedContent<T> {
    content: Option<Content>,
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

fn read_document<T: Deserialize<'de>>(
    extension: &str,
    file_content: &str,
) -> ReadDocumentResult<DocumentWithPossiblyEmbeddedContent<T>> {
    match extension {
        "md" | "markdown" => {
            let matter = Matter::<YAML>::new().parse(file_content);
            let data: T = data.deserialize().unwrap();

            Ok(DocumentWithPossiblyEmbeddedContent {
                content,
                document: matter.content,
            })
        }
        "yml" | "yaml" => Ok(DocumentWithPossiblyEmbeddedContent {
            content: None,
            document: serde_yaml::from_str(file_content)?,
        }),
        "json" => Ok(DocumentWithPossiblyEmbeddedContent {
            content: None,
            document: serde_json::from_str(file_content)?,
        }),
        "toml" => Ok(DocumentWithPossiblyEmbeddedContent {
            content: None,
            document: toml::from_str(file_content)?,
        }),
        _ => Err(NonDocument),
    }
}

fn extract_content_source(
    path: &Path,
    content_meta: Option<file_schema::Content>,
    embedded_content: Option<Content>,
) -> ContentResult<ContentSourceType> {
    if let Some(content) = embedded_content {
        if let Some(_) = content_meta {
            Err("Documents with embedded content cannot reference other sources")
        }
        return Ok(ContentSourceType::Embedded(content));
    }

    fn ref_without_extension(path: &mut Path) -> ContentSourceType {
        ContentSourceType::FileRef(Path::new(path.file_stem()))
    }

    match content_meta {
        None => Ok(ref_without_extension(path)),
        Some(file_schema::Content::EmbeddedPlaintext(plaintext)) => Ok(
            ContentSourceType::Embedded(ContentType::Plaintext, plaintext),
        ),
        Some(file_schema::Content::FileRef { src, .. }) => match src {
            Some(src) => {
                let mut path = PathBuf::from(path);
                path.pop();
                path.push(src);
                Ok(ContentSourceType::FileRef(path))
            }
            None => Ok(ref_without_extension(path)),
        },
    }
}

fn load_content_source(source_type: ContentSourceType) -> ContentResult<Content> {
    match source_type {
        ContentSourceType::Embedded(content_type, raw) => Ok(Content { content_type, raw }),
        ContentSourceType::FileRef(path) => match path.extension() {
            Some("html") | Some("htm") => Content {
                content_type: ContentType::Html,
                raw: read_to_string(path),
            },
            Some("md") | Some("markdown") => Content {
                content_type: ContentType::Markdown,
                raw: read_to_string(path),
            },
            Some("txt") => Content {
                content_type: ContentType::Plaintext,
                raw: read_to_string(path),
            },
            Some(_) | None => Err(format!("Unsupported content at path {}", path))?,
        },
    }
}

fn transform_content(content: Content) -> ContentResult<TransformedContent> {
    match content.content_type {
        ContentType::Plaintext => TransformedContent {
            html: format("<pre>{}</pre>",  html_escape::encode_text(content.raw)),
            assets: vec![]
        },
        ContentType::Markdown => todo!(),
        ContentType::Html => TransformedContent {
            html: content.raw,
            assets: vec![]
        },
    }
}