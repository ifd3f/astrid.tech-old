use std::{error::Error, path::Path};

use git2::*;

pub fn commit_changes(repo_dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    let repo = Repository::open(repo_dir.as_ref())?;
    let mut index = repo.index()?;
    index.add_all(&["."], IndexAddOption::DEFAULT, None)?;
    index.write()?;
    let oid = index.write_tree()?;
    let tree = repo.find_tree(oid)?;
    let signature = Signature::now("Test", "test@astrid.tech")?;

    if repo.is_empty()? {
        repo.commit(
            Some("HEAD"),
            &signature,
            &signature,
            "init commit test",
            &tree,
            &[],
        )?;
    } else {
        let parent_commit = repo
            .head()?
            .resolve()?
            .peel(ObjectType::Commit)?
            .into_commit()
            .map_err(|_| git2::Error::from_str("foo"))?;

        repo.commit(
            Some("HEAD"),
            &signature,
            &signature,
            "Test message",
            &tree,
            &[&parent_commit],
        )?;
    }
    Ok(())
}

pub fn push_git_changes(dir: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    let repo = Repository::open(dir)?;
    let mut remote = repo.find_remote("origin")?;
    remote.connect(Direction::Push)?;
    remote.push(&["refs/heads/main:refs/heads/main"], None)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{
        fs::{self, File},
        io::Write,
        path::PathBuf,
    };

    use git2::Repository;
    use tempdir::TempDir;

    use super::{commit_changes, push_git_changes};

    fn setup_repo_with_remote() -> (TempDir, PathBuf, PathBuf) {
        let tempdir = TempDir::new("wm-receiver-test").unwrap();

        let remote_path = tempdir.path().join("remote");
        let repo = Repository::init(&remote_path).unwrap();

        let subdir_path = remote_path.join("wm_subdir");
        fs::create_dir_all(&subdir_path).unwrap();
        let file_path = subdir_path.join("wm_file");
        File::create(file_path)
            .unwrap()
            .write("Test contents".as_bytes())
            .unwrap();

        commit_changes(&remote_path).unwrap();

        let local_path = tempdir.path().join("local");
        let local_repo = Repository::clone(remote_path.to_str().unwrap(), &local_path).unwrap();
        (tempdir, remote_path, local_path)
    }

    #[test]
    fn commits_and_pushes() {
        let (tempdir, remote, local) = setup_repo_with_remote();

        let subdir_path = local.join("wm_subdir");
        let file_path = subdir_path.join("another_file");
        File::create(file_path)
            .unwrap()
            .write("More ocntents".as_bytes());

        commit_changes(&local).unwrap();
        push_git_changes(&local).unwrap();
    }
}
