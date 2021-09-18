use std::{env::current_dir, error::Error, path::{Path, PathBuf}};

use tokio::process::Command;

fn get_script_command(name: &str) -> Result<PathBuf, Box<dyn Error>> {
    let mut dir = current_dir()?;
    dir.push("resources/scripts");
    dir.push(name);
    Ok(dir)
}

pub async fn reset_dir(repo_dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    let cmd = get_script_command("reset_to_latest.sh")?.into_os_string();
    tokio::fs::create_dir_all(&repo_dir).await?;

    Command::new(cmd)
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
    let cmd = get_script_command("push_to_git.sh")?.into_os_string();
    Command::new(&cmd)
        .arg("Here are some test changes")
        .arg("https://github.com/astralbijection/astrid.tech.git")
        .arg("webmention/test")
        .current_dir(repo_dir)
        .spawn()?
        .wait()
        .await?;
    Ok(())
}
