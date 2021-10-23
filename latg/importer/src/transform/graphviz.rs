use std::process::Stdio;

use tokio::{io::{AsyncReadExt, AsyncWriteExt, BufReader}, process::Command};


pub async fn transform_graphviz_to_svg(graphviz: &str) -> Result<String, Box<dyn std::error::Error>> {
    let mut child = Command::new("dot").arg("-Tsvg").stdin(Stdio::piped()).stdout(Stdio::piped()).spawn()?;
    let mut stdin = child.stdin.take().expect("No stdin found");
    let mut stdout = child.stdout.take().expect("No stdout found");
    let mut reader = BufReader::new(stdout);

    stdin.write_all(graphviz.as_bytes()).await;
    stdin.flush();
    drop(stdin);
    let status = child.wait().await.expect("Child encountered error");
    let mut svg = String::new();
    reader.read_to_string(&mut svg).await;
    return Ok(svg);
}


#[cfg(test)]
mod tests {
    use std::process::Stdio;

    use crate::transform::graphviz::transform_graphviz_to_svg;

    use rstest::rstest;

    const FUNCTIONAL_SMOKE_CASE: &str = r#"digraph G { a -> b }"#;

    #[rstest]
    #[tokio::test]
    async fn test_footnote() {
        let svg = transform_graphviz_to_svg(FUNCTIONAL_SMOKE_CASE).await.unwrap();

        assert!(svg.contains("</svg>"), "Graphviz output: {}", svg);
    }
}
