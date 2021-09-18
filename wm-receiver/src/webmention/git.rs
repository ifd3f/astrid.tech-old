use std::{env::current_dir, error::Error, ffi::OsStr, path::{Path, PathBuf}};

use tokio::process::Command;

fn get_script_command(name: &str) -> Result<PathBuf, Box<dyn Error>> {
    let mut dir = current_dir()?;
    dir.push(env!("WEBMENTION_RESOURCES_DIR"));
    dir.push("scripts");
    dir.push(name);
    Ok(dir)
}

pub async fn reset_dir(
    repo_dir: impl AsRef<Path>,
    remote_url: impl AsRef<OsStr>,
    branch: impl AsRef<OsStr>,
    base_branch: impl AsRef<OsStr>,
) -> Result<(), Box<dyn Error>> {
    let cmd = get_script_command("reset_to_latest.sh")?.into_os_string();
    tokio::fs::create_dir_all(&repo_dir).await?;

    Command::new(cmd)
        .arg(remote_url)
        .arg(branch)
        .arg(base_branch)
        .current_dir(repo_dir)
        .spawn()?
        .wait()
        .await?;
    Ok(())
}

pub async fn push_changes(
    repo_dir: impl AsRef<Path>,
    message: impl AsRef<OsStr>,
    remote_url: impl AsRef<OsStr>,
    branch: impl AsRef<OsStr>,
) -> Result<(), Box<dyn Error>> {
    let cmd = get_script_command("push_to_git.sh")?.into_os_string();
    Command::new(&cmd)
        .arg(message)
        .arg(remote_url)
        .arg(branch)
        .current_dir(repo_dir)
        .spawn()?
        .wait()
        .await?;
    Ok(())
}
