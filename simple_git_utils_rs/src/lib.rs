use std::{error::Error, ffi::OsStr, path::PathBuf};

use tokio::process::Command;

type GResult<T> = Result<T, Box<dyn Error>>;

pub struct ManagedGitRepo {
    repo_dir: PathBuf,
    remote_url: String,
    branch: String,
    base_branch: String,
}

impl ManagedGitRepo {
    pub fn new(
        repo_dir: PathBuf,
        remote_url: String,
        branch: String,
        base_branch: String,
    ) -> ManagedGitRepo {
        ManagedGitRepo {
            repo_dir,
            remote_url,
            branch,
            base_branch,
        }
    }

    async fn git(&self, args: impl IntoIterator<Item = impl AsRef<OsStr>>) -> GResult<()> {
        Command::new("git").args(args).current_dir(&self.repo_dir).spawn()?.wait().await?;
        Ok(())
    }

    async fn fetch(&self) -> GResult<()> {
        self.git(["fetch", "origin", "-pP", "--depth=1", &self.base_branch]).await?;
        self.git(["fetch", "origin", "-pP", "--depth=1", &self.branch]).await?;
        Ok(())
    }

    pub async fn reset_dir(&mut self) -> GResult<()> {
        // Ensure git repository exists
        self.git(["init"]).await?;

        // Clean the directory
        self.git(["add", "-A"]).await?;
        self.git(["reset", "--hard"]).await?;

        // Ensure that origin points to the remote so we can fetch the data
        self.git(["remote", "add", "origin", &self.remote_url]).await?;
        self.git(["remote", "set-url", "origin", &self.remote_url]).await?;

        // Fetch remote data
        self.fetch().await?;

        // In case this is our first time on this repository, check out main
        self.git(["checkout", &self.base_branch]).await?;

        // Ensure work branch exists and check it out. Deleting first ensures we 
        self.git(["branch", "-d", &self.branch]).await?;
        self.git(["checkout", "--guess", &self.branch]).await?;
        self.git(["checkout", "-b", &self.branch]).await?;

        Ok(())
    }

    pub async fn push_changes(&mut self, message: &str) -> Result<(), Box<dyn Error>> {
        // Add all files and commit
        self.git(["add", "-A"]).await?;
        self.git(["commit", "-m", message]).await?;

        self.fetch().await?;

        // Push to the specified remote
        self.git(["rebase", &format!("origin/{}", self.branch)]).await?;
        self.git(["push"]).await?;

        Ok(())
    }
}
