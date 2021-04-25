mod markdown;
mod cms_types;

use std::fs::read_to_string;

use comrak::{Arena, ComrakOptions, format_html, parse_document};
use comrak::nodes::AstNode;
use gray_matter::engine::yaml::YAML;
use gray_matter::matter::Matter;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
struct SomeFrontMatter {
    description: String,
    title: String,
}

fn main() {
    let raw_md = read_to_string("testfile.md").unwrap();
    let mut matter: Matter<YAML> = Matter::new();
    let yaml = matter.matter_struct::<SomeFrontMatter>(raw_md);

    let arena = Arena::new();

    let root = parse_document(
        &arena,
        yaml.content.as_str(),
        &ComrakOptions::default());

    let mut s = Vec::new();
    format_html(root, &ComrakOptions::default(), &mut s).unwrap();

    let written = String::from_utf8(s).unwrap();

    println!("{:#?}", root);

    println!("{:?}\n{}", yaml.data, written);
}
