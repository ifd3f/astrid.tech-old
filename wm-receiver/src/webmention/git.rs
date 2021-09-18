use std::{error::Error, path::Path};

use tokio::process::Command;

pub async fn reset_dir(repo_dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    let result = Command::new("./resources/scripts/reset_to_latest.sh")
        .arg("https://github.com/astralbijection/astrid.tech.git")
        .arg("webmention/test")
        .arg("main")
        .current_dir(repo_dir)
        .spawn()?
        .wait()
        .await?;
    Ok(())
}

pub async fn push_changes(repo_dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    let result = Command::new("./resources/scripts/push_to_git.sh")
        .arg("My test changes")
        .arg("https://github.com/astralbijection/astrid.tech.git")
        .arg("webmention/test")
        .current_dir(repo_dir)
        .spawn()?
        .wait()
        .await?;
    Ok(())
}
