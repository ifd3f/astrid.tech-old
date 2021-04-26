use std::fs::read_to_string;

use comrak::{Arena, ComrakOptions, format_html, parse_document};
use comrak::nodes::AstNode;
use gray_matter::engine::yaml::YAML;
use gray_matter::matter::Matter;
use serde::{Deserialize, Serialize};
use derive_more::{From};

struct ParseResult<T> {
    meta: T,
    content_html: String,
    content_md: String,
}

static matter: Matter<YAML> = Matter::new();

#[derive(Debug, From)]
enum MarkdownReadError {
    IOError(std::io::Error),
    UTF8ConversionError(std::string::FromUtf8Error),
}

fn read_md_from_file<T: serde::de::DeserializeOwned>(
    path: &str) -> Result<ParseResult<T>, MarkdownReadError> {
    let raw_md = read_to_string(path)?;

    let parsed = matter.matter_struct::<T>(raw_md);

    let arena = Arena::new();
    let root = parse_document(
        &arena,
        parsed.content.as_str(),
        &ComrakOptions::default());

    // TODO: customize

    let mut buffer = Vec::new();
    format_html(root, &ComrakOptions::default(), &mut buffer)?;

    Ok(ParseResult {
        meta: parsed.data,
        content_md: parsed.content,
        content_html: String::from_utf8(buffer).unwrap(),
    })
}