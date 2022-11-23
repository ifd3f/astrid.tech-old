#!/usr/bin/env python3

import re
import csv
import sys
from typing import Dict, Tuple, Iterable
from pathlib import Path


def main():
    uploads = Path(sys.argv[1])
    root = Path(sys.argv[2])
    with uploads.open('r') as f:
        table = {
            r['file']: r['upload_url']
            for r in csv.DictReader(f)
        }
    
    mod_md_recursive(table, root, root)


def mod_md_recursive(table: Dict[str, str], path: Path, root: Path) -> Iterable[Tuple[Path, str]]:
    if path.is_file():
        if path.suffix == '.md':
            mod_md(table, path, root)
        return

    for child in path.iterdir():
        mod_md_recursive(table, child, root)


def mod_md(table: Dict[str, str], path: Path, root: Path):
    print(f'Modifying {path}')
    with path.open('r') as f:
        contents = f.read()

    modlist = []
    for match in re.finditer(r'\!\[.*\]\((.*)\)|thumbnail: (.*)', contents):
        url = match.group(1) or match.group(2)
        if url.startswith(('http://', 'https://')):
            continue
        if url.startswith('/'):
            fs_path = root / url[1:]
        elif url.startswith('/_/'):
            fs_path = root / url[3:]
        else:
            fs_path = (path.parent / url).relative_to(root)

        new_url = table.get(str(fs_path))
        if new_url is not None:
            modlist.append((url, new_url))

    for fr, to in modlist:
        contents = contents.replace(fr, to)
    
    with path.open('w') as f:
        f.write(contents)


if __name__ == '__main__':
    main()
