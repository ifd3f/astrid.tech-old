use std::path::{Path, PathBuf};

pub fn get_resources_path(child: &str) -> PathBuf {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("resources/test");
    d.push(child);
    d
}

pub fn get_temp_path(child: &str) -> PathBuf {
    let mut d = std::env::temp_dir();
    d.push("astrid.tech/test");
    d.push(child);
    std::fs::remove_dir_all(d.as_path());
    std::fs::create_dir_all(d.as_path()).unwrap();
    d
}