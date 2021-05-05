use std::fs::File;
use std::io::Error;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::page::MetaParseError::IOError;

pub enum DocumentType {
    Markdown,
    Jupyter,
}

impl DocumentType {
    pub fn from_ext(ext: &str) -> Option<DocumentType> {
        match ext {
            "md" => Some(DocumentType::Markdown),
            "ipynb" => Some(DocumentType::Jupyter),
            _ => None,
        }
    }
}

pub enum MetaType {
    JSON,
    YAML,
}

pub enum MetaParseError {
    YAMLError(serde_yaml::Error),
    IOError(std::io::Error),
}

impl From<std::io::Error> for MetaParseError {
    fn from(e: Error) -> Self {
        MetaParseError::IOError(e)
    }
}

impl From<serde_yaml::Error> for MetaParseError {
    fn from(e: serde_yaml::Error) -> Self {
        MetaParseError::YAMLError(e)
    }
}

impl MetaType {
    pub fn from_ext(ext: &str) -> Option<MetaType> {
        match ext {
            "json" => Some(MetaType::JSON),
            "yaml" => Some(MetaType::YAML),
            _ => None
        }
    }

    pub fn parse<T: for<'de> Deserialize<'de>>(&self, path: &Path) -> Result<T, MetaParseError> {
        let file = File::open(path)?;
        match self {
            MetaType::JSON => panic!("JSON NYI"),
            MetaType::YAML => Ok(serde_yaml::from_reader(file)?)
        }
    }
}

pub struct Page {
    pub short_name: String,
    pub doctype: DocumentType,
    pub content: String,
    pub asset_dir: PathBuf,
}