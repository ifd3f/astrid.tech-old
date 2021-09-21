use std::{env::current_dir, error::Error, ffi::OsStr, path::PathBuf};

#[cfg(test)]
extern crate rstest;

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

    pub fn reset_dir(&mut self) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    pub fn push_changes(&mut self, message: impl AsRef<OsStr>) -> Result<(), Box<dyn Error>> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use git2::Repository;
    use rstest::rstest;
    use std::error::Error;
    use std::process::Command;

    use std::sync::Once;

    static INIT: Once = Once::new();

    pub fn setup_fresh_gittest() -> () {
        INIT.call_once(|| {
            Command::new("./setup-gittest.sh")
                .status()
                .expect("Failed to set up gittest");
        });
    }

    #[test]
    fn it_works() {
        setup_fresh_gittest();
        let repo = Repository::open("gittest/local").unwrap();
    }
}
