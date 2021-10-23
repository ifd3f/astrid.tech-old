use std::process::Stdio;

use tokio::{
    io::{AsyncReadExt, AsyncWriteExt, BufReader},
    process::Command,
};

#[derive(Debug, thiserror::Error)]
#[error("Invalid Graphviz input")]
struct InvalidGraphviz;

pub async fn transform_graphviz_to_svg(
    graphviz: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut child = Command::new("dot")
        .arg("-Tsvg")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let stdout = child.stdout.take().expect("No stdout found");
    let mut reader = BufReader::new(stdout);

    {
        let mut stdin = child.stdin.take().expect("No stdin found");
        stdin.write_all(graphviz.as_bytes()).await?;
        stdin.flush().await?;
    }

    let status = child.wait().await?;

    let mut svg = String::new();
    reader.read_to_string(&mut svg).await?;
    if (svg.is_empty()) {
        Err(InvalidGraphviz)?;
    }

    Ok(svg)
}

#[cfg(test)]
mod tests {
    use std::{assert_matches::assert_matches, process::Stdio};

    use crate::transform::graphviz::transform_graphviz_to_svg;

    use rstest::rstest;

    const VALID_DIGRAPH: &str = r#"digraph G { a -> b }"#;
    const INVALID_DIGRAPH: &str = r#"digraph G { a -> b"#;

    #[rstest]
    #[tokio::test]
    async fn outputs_svg_on_valid_graphviz() {
        let svg = transform_graphviz_to_svg(VALID_DIGRAPH).await.unwrap();

        assert!(svg.contains("</svg>"), "Graphviz output: {}", svg);
    }

    #[rstest]
    #[tokio::test]
    async fn fails_on_invalid_graphviz() {
        let result = transform_graphviz_to_svg(INVALID_DIGRAPH).await;

        assert_matches!(result, Err(_));
    }
}
