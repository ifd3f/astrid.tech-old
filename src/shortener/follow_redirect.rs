use scraper::{Html, Selector};

pub async fn follow_redirect(uri: String, max_times: u32) -> String {
    if max_times == 0 {
        return uri;
    }

    uri
}

pub fn extract_meta_redirect(document: &'a str) -> Option<String> {
    let result = Html::parse_document(document);

    // Finds tags that look like <meta http-equiv="refresh" content="2;url=http://example.com" />
    let selector = Selector::parse(r#"meta[http-equiv="refresh"]"#).unwrap();
    let meta = match result.select(&selector).next() {
        Some(element) => element.value(),
        None => return None,
    };

    // Extracts the content attribute of the tag
    let content = match meta.attr("content") {
        Some(content) => content,
        None => return None,
    };

    static PATTERN: &str = ";url=";
    let (refresh, uri) = match content.find(PATTERN) {
        Some(i) => {
            let seconds = content[0..(i - 1)].parse().unwrap_or(0);
            let uri = &content[(i + PATTERN.len())..];
            (seconds, uri)
        }
        None => return None,
    };

    if refresh < 5 {
        Some(uri.to_string())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use rstest::*;
    use std::fs;
    use std::path::{Path, PathBuf};
    use crate::shortener::follow_redirect::extract_meta_redirect;

    #[fixture]
    fn test_resources_dir() -> PathBuf {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("resources/test");
        d
    }

    #[rstest(name)]
    #[case("simple-redirect")]
    fn extract_meta_redirect_exists(name: &str, test_resources_dir: PathBuf) {
        let mut path = test_resources_dir.clone();
        path.push("redirect");
        let input =
            fs::read_to_string(format!("{}/{}.html", path.to_str().unwrap(), name)).unwrap();
        let expected =
            fs::read_to_string(format!("{}/{}.txt", path.to_str().unwrap(), name)).unwrap();

        let actual = extract_meta_redirect(input.as_str());

        assert_eq!(actual, Some(expected))
    }

    #[rstest(name)]
    #[case("long-refresh")]
    #[case("no-meta")]
    fn extract_meta_redirect_not_exists(name: &str, test_resources_dir: PathBuf) {
        let mut path = test_resources_dir.clone();
        path.push("noredirect");
        let input =
            fs::read_to_string(format!("{}/{}.html", path.to_str().unwrap(), name)).unwrap();

        let actual = extract_meta_redirect(input.as_str());

        assert_eq!(actual, None)
    }
}
