use std::ffi::OsStr;
use std::fs;
use std::fs::{File, metadata};
use std::io;
use std::path::{Path, PathBuf};

use gray_matter::engine::yaml::YAML;
use gray_matter::matter::Matter;
use gray_matter::value::pod::Pod;
use serde::de::Error;
use yaml_rust::ScanError;

/// Everything that could possibly go wrong if you were to load a document.
#[derive(Debug)]
pub enum DocumentLoadError {
    InvalidPath(PathBuf),
    NotAFileOrDirectory(PathBuf),
    NoIndex,
    AmbiguousIndex(Vec<PathBuf>),
    AmbiguousMeta(Vec<PathBuf>),
    UnknownDocumentType(String),
    UnknownMetaType(String),
    IOError(io::Error),
    SerdeError(serde_json::error::Error),
}

pub type DocumentResult<T> = Result<T, DocumentLoadError>;

impl From<io::Error> for DocumentLoadError {
    fn from(e: io::Error) -> DocumentLoadError {
        DocumentLoadError::IOError(e)
    }
}

impl From<serde_json::error::Error> for DocumentLoadError {
    fn from(e: serde_json::error::Error) -> Self {
        DocumentLoadError::SerdeError(e)
    }
}

pub enum DocumentType {
    Markdown,
    Jupyter,
}

pub struct Document<T> {
    short_name: String,
    doctype: DocumentType,
    content: String,
    meta: Option<T>,
}

impl<T: for<'de> serde::Deserialize<'de>> Document<T> {
    /// Directly load a markdown file.
    fn load_markdown(
        parts: DocumentParts
    ) -> DocumentResult<Document<T>> {
        let matter = Matter::<YAML>::new();
        let file_contents = fs::read_to_string(parts.main_file.clone())?;
        let parsed_matter = matter.matter(file_contents);

        let meta: Option<T> = match parsed_matter.data {
            Pod::Null => None,
            p => Some(p.deserialize()?)
        };

        let meta = match (meta, parts.meta) {
            (None, None) => Ok(None),
            (Some(m), None) => Ok(Some(m)),
            (None, Some(p)) => panic!("Detached meta NYI"),
            (_, Some(p)) => Err(DocumentLoadError::AmbiguousMeta(vec![parts.main_file, p]))
        }?;

        Ok(Document {
            short_name: parts.short_name,
            doctype: DocumentType::Markdown,
            meta,
            content: parsed_matter.content.to_string(),
        })
    }

    /// Load the document at this path.
    pub fn load(parts: DocumentParts) -> DocumentResult<Document<T>> {
        let ext = parts.main_file.extension()
            .and_then(|s| s.to_str());

        let doctype = match ext {
            Some("md") => Ok(DocumentType::Markdown),
            Some("ipynb") => Ok(DocumentType::Jupyter),
            Some(ext) => Err(DocumentLoadError::UnknownDocumentType(ext.to_string())),
            None => Err(DocumentLoadError::UnknownDocumentType("".to_string()))
        }?;

        match doctype {
            Markdown => Self::load_markdown(parts),
            Jupyter => panic!("Not yet implemented!")
        }
    }
}

#[derive(Debug, Clone)]
pub struct DocumentParts {
    short_name: String,
    main_file: PathBuf,
    meta: Option<PathBuf>,
}

impl DocumentParts {
    fn load_from_folder(path: &Path) -> DocumentResult<DocumentParts> {
        let short_name = path.file_name()
            .and_then(|s| s.to_str())
            .map_or(
                Err(DocumentLoadError::InvalidPath(PathBuf::from(path))),
                |s| Ok(s.to_string()),
            )?;

        let index = {
            let mut candidates = find_file_with_stem(OsStr::new("index"), path)?;
            if candidates.len() > 1 {
                Err(DocumentLoadError::AmbiguousIndex(candidates))
            } else {
                candidates.pop().map_or(Err(DocumentLoadError::NoIndex), |p| Ok(p))
            }
        }?;
        let meta = {
            let mut candidates = find_file_with_stem(OsStr::new("meta"), path)?;
            if candidates.len() > 1 {
                Err(DocumentLoadError::AmbiguousMeta(candidates))
            } else {
                Ok(candidates.pop())
            }
        }?;

        Ok(DocumentParts {
            short_name,
            main_file: index,
            meta,
        })
    }

    fn load_from_file(path: &Path) -> DocumentResult<DocumentParts> {
        let short_name = path.file_stem()
            .and_then(|s| s.to_str())
            .map_or(Err(DocumentLoadError::InvalidPath(PathBuf::from(path))), |s| Ok(s.to_string()))?;
        Ok(DocumentParts {
            short_name,
            main_file: PathBuf::from(path),
            meta: None,
        })
    }

    pub fn load(path: &Path) -> DocumentResult<DocumentParts> {
        let fs_meta = fs::metadata(path)?;

        if fs_meta.is_dir() {
            DocumentParts::load_from_folder(path)
        } else if fs_meta.is_file() {
            DocumentParts::load_from_file(path)
        } else {
            Err(DocumentLoadError::NotAFileOrDirectory(PathBuf::from(path)))
        }
    }
}

fn find_file_with_stem(file_stem: &OsStr, path: &Path) -> Result<Vec<PathBuf>, DocumentLoadError> {
    let data = fs::read_dir(path)?
        .filter_map(|e| e.ok())  // Only examine "ok" files
        .filter_map(|e| {
            // Only when the file stem is "index"
            let file_name = e.file_name();
            let mut name = PathBuf::new();
            name.push(path);
            name.push(file_name);
            if name.file_stem() == Some(file_stem) {
                Some(name)
            } else {
                None
            }
        }).collect();
    Ok(data)
}

#[cfg(test)]
mod test {
    use at_objects::input_types::{ArticleMeta, ProjectMeta};

    use crate::document::{Document, DocumentParts};
    use crate::document::DocumentLoadError::*;
    use crate::test_util::get_resources_path;

    #[test]
    fn loads_markdown_with_meta() {
        let parts = DocumentParts {
            short_name: "some-name".to_string(),
            main_file: get_resources_path("blog-posts/site-release.md"),
            meta: None,
        };

        let doc = Document::<ArticleMeta>::load(parts.clone()).unwrap();

        assert_eq!(doc.short_name, parts.short_name);
    }

    #[test]
    fn finds_separate_meta() {
        let path = get_resources_path("blog-posts/separate-meta/");
        let path = path.as_path();

        let detected = DocumentParts::load(path).unwrap();

        assert_eq!(detected.meta, Some(get_resources_path("blog-posts/separate-meta/meta.yaml")));
        assert_eq!(detected.main_file, get_resources_path("blog-posts/separate-meta/index.md"));
        assert_eq!(detected.short_name, "separate-meta");
    }

    #[test]
    fn finds_single_index_in_folder() {
        let path = get_resources_path("blog-posts/one");
        let path = path.as_path();

        let detected = DocumentParts::load(path).unwrap();

        assert_eq!(detected.main_file, get_resources_path("blog-posts/one/index.md"));
        assert_eq!(detected.short_name, "one");
        assert_eq!(detected.meta, None);
    }

    #[test]
    fn finds_single_file_post() {
        let path = get_resources_path("blog-posts/this-file.md");
        let path = path.as_path();

        let detected = DocumentParts::load(path).unwrap();

        assert_eq!(detected.main_file, get_resources_path("blog-posts/this-file.md"));
        assert_eq!(detected.short_name, "this-file");
        assert_eq!(detected.meta, None);
    }

    #[test]
    fn errors_on_multiple_meta() {
        let path = get_resources_path("blog-posts/many-meta");
        let path = path.as_path();

        let detected = DocumentParts::load(path);

        assert_matches!(detected, Err(AmbiguousMeta(_)));
    }

    #[test]
    fn errors_on_multiple_index() {
        let path = get_resources_path("blog-posts/many-index");
        let path = path.as_path();

        let detected = DocumentParts::load(path);

        assert_matches!(detected, Err(AmbiguousIndex(_)));
    }

    #[test]
    fn errors_on_no_index_in_folder() {
        let path = get_resources_path("blog-posts/none");
        let path = path.as_path();

        let detected = DocumentParts::load(path);

        assert_matches!(detected, Err(NoIndex));
    }
}