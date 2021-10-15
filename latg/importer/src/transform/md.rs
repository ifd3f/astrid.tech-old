use std::path::Path;

use pulldown_cmark::{html, Options, Parser};

pub fn transform_markdown(path: impl AsRef<Path>, markdown: impl AsRef<str>) -> String {
    let mut options = Options::all();
    let parser = Parser::new_ext(markdown.as_ref(), options);

    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    html_output
}

#[cfg(test)]
mod tests {
    use crate::transform::md::transform_markdown;

    use rstest::rstest;

    const FOOTNOTE_CASE: &str = r#"
All according to keikaku[^1]!

[^1]: keikaku means plan
"#;

    #[rstest]
    fn test_footnote() {
        let html = transform_markdown("foo", FOOTNOTE_CASE);

        assert_eq!(html, "f");
    }
}
