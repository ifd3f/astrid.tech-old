use std::ffi::OsStr;
use std::fs;
use std::fs::{File, metadata};
use std::io;
use std::path::{Path, PathBuf};

use yaml_rust::ScanError;

use crate::document::DocumentLoadError::{AmbiguousIndex, InvalidPath, NoIndex};
use crate::document::DocumentType::{Jupyter, Markdown};

/// Everything that could possibly go wrong if you were to load a document.
pub enum DocumentLoadError {
    InvalidPath(PathBuf),
    NotAFileOrDirectory(PathBuf),
    NoIndex,
    AmbiguousIndex(Vec<PathBuf>),
    AmbiguousMeta(Vec<PathBuf>),
    UnknownDocumentType(String),
    UnknownMetaType(String),
    IOError(io::Error),
    YAMLSyntaxError(serde_yaml::Error),
    YAMLScanError(yaml_rust::scanner::ScanError),
}

pub type DocumentResult<T> = Result<T, DocumentLoadError>;

impl From<io::Error> for DocumentLoadError {
    fn from(e: io::Error) -> DocumentLoadError {
        DocumentLoadError::IOError(e)
    }
}

impl From<serde_yaml::Error> for DocumentLoadError {
    fn from(e: serde_yaml::Error) -> DocumentLoadError {
        DocumentLoadError::YAMLSyntaxError(e)
    }
}

impl From<yaml_rust::scanner::ScanError> for DocumentLoadError {
    fn from(e: ScanError) -> Self {
        DocumentLoadError::YAMLScanError(e)
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
        short_name: String,
        path: &Path,
    ) -> DocumentResult<Document<T>> {
        let data = fs::read_to_string(path)?;
        let (opt_fm, content) = frontmatter::parse_and_find_content(data.as_str())?;

        let meta = opt_fm
            .map(|yaml| {
                let str = yaml.as_str().unwrap();
                serde_yaml::from_str::<T>(str)
            })
            .map_or(Ok(None), |e| e.map(Some))?;

        Ok(Document {
            short_name: short_name.to_string(),
            doctype: DocumentType::Markdown,
            meta,
            content: content.to_string(),
        })
    }

    /// Load the document at this path.
    pub fn load(
        path: &Path) -> DocumentResult<Document<T>> {
        let parts = DocumentParts::load(path)?;

        let ext = parts.main_file.extension()
            .and_then(|s| s.to_str());

        let doctype = match ext {
            Some("md") => Ok(Markdown),
            Some("ipynb") => Ok(Jupyter),
            Some(ext) => Err(DocumentLoadError::UnknownDocumentType(ext.to_string())),
            None => Err(DocumentLoadError::UnknownDocumentType("".to_string()))
        }?;

        match doctype {
            Markdown => Self::load_markdown(
                parts.short_name,
                parts.main_file.as_ref(),
            ),
            Jupyter => panic!("Not yet implemented!")
        }
    }
}

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
                |s| Ok(s.to_string())
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
            .map_or(Err(InvalidPath(PathBuf::from(path))), |s| Ok(s.to_string()))?;
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