use std::{env::current_dir, error::Error, ffi::OsStr, path::PathBuf};

use tokio::process::Command;

fn get_script_command(name: &str) -> Result<PathBuf, Box<dyn Error>> {
    let mut dir = current_dir()?;
    dir.push(env!("WEBMENTION_RESOURCES_DIR"));
    dir.push("scripts");
    dir.push(name);
    Ok(dir)
}

pub struct ManagedGitRepo {
    repo_dir: PathBuf,
    remote_url: String,
    branch: String,
    base_branch: String
}

impl ManagedGitRepo {
    pub fn new(repo_dir: PathBuf, remote_url: String, branch: String, base_branch: String) -> ManagedGitRepo {
        ManagedGitRepo {
            repo_dir,
            remote_url,
            branch,
            base_branch
        }
    }

    pub async fn reset_dir(
        &mut self,
    ) -> Result<(), Box<dyn Error>> {
        let cmd = get_script_command("reset_to_latest.sh")?.into_os_string();
        tokio::fs::create_dir_all(&self.repo_dir).await?;

        Command::new(cmd)
            .arg(&self.remote_url)
            .arg(&self.branch)
            .arg(&self.base_branch)
            .current_dir(&self.repo_dir)
            .spawn()?
            .wait()
            .await?;
        Ok(())
    }

    pub async fn push_changes(
        &mut self,
        message: impl AsRef<OsStr>,
    ) -> Result<(), Box<dyn Error>> {
        let cmd = get_script_command("push_to_git.sh")?.into_os_string();
        Command::new(&cmd)
            .arg(message)
            .arg(&self.remote_url)
            .arg(&self.branch)
            .current_dir(&self.repo_dir)
            .spawn()?
            .wait()
            .await?;
        Ok(())
    }
}
