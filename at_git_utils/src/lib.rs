use shellfn::shell;
use std::error::Error;

#[shell]
fn reset_to_latest(
    dir: &str,
    remote_url: &str,
    work_branch: &str,
    main_branch: &str,
) -> Result<(), Box<dyn Error>> {
    r#"
    cd dir

    #!/usr/bin/env sh

    # Ensure git repository exists
    git init

    # Clean the directory
    git add -A
    git reset --hard

    # Ensure that origin points to the remote so we can fetch the data
    git remote add origin $REMOTE || git remote set-url origin $REMOTE

    # Fetch the work branch, or the main branch as a fallback
    git fetch origin --depth 1 $WORK_BRANCH || git fetch origin --depth 1 $MAIN_BRANCH

    # Delete the cached origin tags if needed
    git remote prune origin  

    # In case this is our first time on this repository, check out main
    git branch $MAIN_BRANCH
    git checkout $MAIN_BRANCH

    # Ensure work branch exists and check it out
    git branch $WORK_BRANCH
    git checkout $WORK_BRANCH

    # Reset to remote work branch, or the main branch if the work branch doesn't exist
    git reset --hard origin/$WORK_BRANCH || git reset --hard origin/$MAIN_BRANCH
    "#
}

#[shell]
fn push_changes(dir: &str, msg: &str, remote: &str, branch: &str) -> Result<(), Box<dyn Error>> {
    r#"
    cd dir

    # Add all files and commit
    git add -A
    git commit -m "$MSG"

    # Push to the specified remote
    git remote set-url origin "$REMOTE"
    git push -f origin "$BRANCH"
    "#
}

pub struct ManagedGitRepo {
    repo_dir: String,
    remote_url: String,
    branch: String,
    base_branch: String,
}

impl ManagedGitRepo {
    pub fn new(
        repo_dir: String,
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

    pub async fn reset_dir(&mut self) -> Result<(), Box<dyn Error>> {
        reset_to_latest(
            self.repo_dir.as_str(),
            self.remote_url.as_str(),
            self.branch.as_str(),
            self.base_branch.as_str(),
        )?;
        Ok(())
    }

    pub async fn push_changes(&mut self, message: impl AsRef<str>) -> Result<(), Box<dyn Error>> {
        push_changes(
            self.repo_dir.as_str(),
            message.as_ref(),
            self.remote_url.as_str(),
            self.branch.as_str(),
        )?;
        Ok(())
    }
}
