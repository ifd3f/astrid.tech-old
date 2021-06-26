"""
Quick n' dirty script for uploading images from my old format.
"""
from os.path import splitext
from pathlib import Path

import click
from dotenv import load_dotenv

from util import create_auth_session

document_files = {'.md', '.ipynb', '.txt'}


@click.command()
@click.argument('content_dir')
def main(content_dir):
    content_dir = Path(content_dir)
    s = create_auth_session()

    for child in content_dir.glob('**/*'):
        if child.is_dir():
            continue

        name, ext = splitext(child.name)
        if ext not in document_files:
            print(f'Uploading {child}')
            with child.open('rb') as fp:
                s.post('https://api.astrid.tech/api/micropub/media', files={'file': fp})


if __name__ == '__main__':
    load_dotenv()
    main()
