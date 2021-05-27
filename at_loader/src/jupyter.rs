use crate::filesystem::FilesystemSet;
use crate::util::walk_extension;
use vfs::VfsResult;

fn jupyter_to_md(fss: FilesystemSet) -> VfsResult<()>{
    for path in walk_extension( "ipynb", fss.get_raw_read_path())? {
        let mut pandoc = pandoc::new();
        pandoc.add_input(path)
    }
    Ok(())
}