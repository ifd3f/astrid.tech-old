use std::collections::HashMap;
use std::fs;
use std::fs::{DirEntry, read_dir, ReadDir};
use std::io::Error;
use std::path::Path;

enum TimestreamContent {
    Article,
    Note,
    RSVP,
}

pub struct TimestreamObject {
    content: TimestreamContent
}

impl TimestreamObject {
    pub fn read_timestream(path: &Path) -> () {

    }
}