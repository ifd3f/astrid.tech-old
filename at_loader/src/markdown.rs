use crate::filesystem::FilesystemSet;

pub struct Markdown {

}

pub struct MarkdownManager {
    objects: Markdown
}

impl MarkdownManager {
    // fn build(filesystem: FilesystemSet) {
    //     let md_files = filesystem.get_filesystem().walk_dir()?
    //         .filter_map(|result| result.ok())
    //         .filter(|path| path.extension().map_or_else(false, |ext| ext == "md"));
    //
    //     for file in md_files {
    //         file.open_file()
    //     }
    // }
}