"""
Quick n' dirty script for uploading posts from my old format.
"""
from datetime import datetime, date
from os.path import splitext
from pathlib import Path

import click
import frontmatter
import pytz
from dotenv import load_dotenv

from util import create_auth_session


def markdown_to_micropub(path: Path):
    if path.name.endswith('.note.md'):
        day = path.parent
        month = day.parent
        year = month.parent
        dt = datetime(int(year.name), int(month.name), int(day.name), tzinfo=pytz.utc).isoformat()

        with path.open('r') as fp:
            content = fp.read()
        return {
            'type': ['h-entry'],
            'properties': {
                'content': [content],
                'created': dt,
            }
        }

    with path.open('r') as fp:
        post = frontmatter.load(fp)
    dt = post['date']
    if isinstance(dt, datetime):
        dt = dt.isoformat()
    if isinstance(dt, date):
        dt = dt.isoformat()

    thumb = post.get('thumbnail')
    if thumb is None:
        thumb = []
    else:
        thumb = [thumb]
    return {
        "type": ["h-entry"],
        "properties": {
            'name': [post.get('title')],
            'summary': [post.get('description')],
            'content': [post.content],
            'category': post.get('tags', []),
            'created': dt,
            'published': dt,
            'photo': thumb
        }
    }


@click.command()
@click.argument('content_dir')
def main(content_dir):
    content_dir = Path(content_dir)
    s = create_auth_session()

    for child in content_dir.glob('**/*'):
        if child.is_dir():
            continue

        name, ext = splitext(child.name)
        if ext == '.md' and not child.name.endswith('.recipe.md'):
            print(f'Reading {child}')
            data = markdown_to_micropub(child)
            print('Has data', data)
            response = s.post('https://api.astrid.tech/api/micropub/', json=data)
            assert response.status_code == 201, response.content


if __name__ == '__main__':
    load_dotenv()
    main()
