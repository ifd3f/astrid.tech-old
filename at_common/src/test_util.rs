use std::path::{Path, PathBuf};

pub fn get_resources_path(child: &str) -> PathBuf {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("resources/test");
    d.push(child);
    d
}