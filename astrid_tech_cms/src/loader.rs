use std::fs;
use std::io;

fn find_frontmatter_and_content(file_contents: &str) -> (Option<&str>, &str) {
    if !file_contents.starts_with("---\n") {
        return (None, file_contents);
    }
    let start = &file_contents[4..];
    match start.find("\n---\n") {
        Some(i) => (Some(&start[..i]), &start[(i + 5)..]),
        None => (None, file_contents),
    }
}

#[test]
fn find_frontmatter_and_content_works_1() {
    assert_eq!(
        find_frontmatter_and_content(
            "---
a: b
---
# Markdown doc begins here
"
        ),
        (Some("a: b"), "# Markdown doc begins here\n")
    )
}

#[test]
fn find_frontmatter_and_content_works_2() {
    let data = "Markdown already began, dude";
    assert_eq!(find_frontmatter_and_content(data), (None, data))
}

pub enum MarkdownLoadError {
    IOError(io::Error),
    YAMLSyntaxError(serde_yaml::Error),
}

impl From<io::Error> for MarkdownLoadError {
    fn from(e: io::Error) -> MarkdownLoadError {
        MarkdownLoadError::IOError(e)
    }
}

impl From<serde_yaml::Error> for MarkdownLoadError {
    fn from(e: serde_yaml::Error) -> MarkdownLoadError {
        MarkdownLoadError::YAMLSyntaxError(e)
    }
}

pub fn load_markdown<Fm: for<'de> serde::Deserialize<'de>>(
    path: &str,
) -> Result<(Option<Fm>, String), MarkdownLoadError> {
    let data = fs::read_to_string(path)?;
    let (opt_fm, content) = find_frontmatter_and_content(&data[..]);
    let x = match opt_fm {
        Some(r) => Some(serde_yaml::from_str::<Fm>(r)?),
        None => None,
    };
    Ok((x, content.to_owned()))
}
