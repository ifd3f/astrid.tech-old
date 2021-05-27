use vfs::{VfsPath, VfsResult};

pub fn walk_extension(ext: &str, path: &'a VfsPath) -> VfsResult<impl Iterator<Item=&'a VfsPath>> {
    Ok(path.walk_dir()?
        .filter_map(|result| result.ok())
        .filter(|path| path.extension().map_or(false, |e| e == ext)))
}