#!/usr/bin/env python3

import logging
import boto3
import click
from botocore.exceptions import ClientError
from botocore.config import Config
from dotenv import load_dotenv
from mimetypes import guess_type

from typing import Tuple, Iterable
from pathlib import Path
from dotenv import load_dotenv
import hashlib
import os
import base64
import sys
import csv


UPLOAD_EXTENSIONS = {'.jpg', '.jpeg', '.png', '.ipynb', '.mp4', '.gif'}
log = logging.getLogger(__name__)


def main():
    load_dotenv()
    cli()


@click.group()
def cli():
    """B2 uploading script."""


@cli.command()
@click.option('-v', '--verbose', count=True)
@click.argument('root', type=click.Path(exists=True))
def upload_all(root: str, verbose: int):
    """Uploads every file in the given directory"""
    logging.basicConfig(level=logging.DEBUG if verbose >= 1 else logging.INFO)
    bucket = get_bucket()

    with open("uploadresult.csv", "w") as f:
        writer = csv.writer(f)
        writer.writerow(['file', 'upload_url'])
        for path, key in upload_all_items(bucket, Path(root)):
            row = [str(path.relative_to(root)), f'https://s3.us-west-000.backblazeb2.com/nyaabucket/{key}']
            writer.writerow(row)


def get_bucket():
    b2 = boto3.resource(
        service_name='s3',
        endpoint_url='https://s3.us-west-000.backblazeb2.com',
        aws_access_key_id=os.environ['B2_ACCESS'],
        aws_secret_access_key=os.environ['B2_SECRET'],
        config=Config(signature_version='s3v4'),
    )
    return b2.Bucket('nyaabucket')


def upload_all_items(bucket, path: Path) -> Iterable[Tuple[Path, str]]:
    if path.is_file():
        if path.suffix not in UPLOAD_EXTENSIONS:
            log.debug("Skipping %s", path)
            return
        yield path, upload_image(bucket, path)
        return

    for child in path.iterdir():
        yield from upload_all_items(bucket, child)


def upload_image(bucket, path: Path) -> str:
    ct, ce = guess_type(str(path))

    h = hashlib.new('sha256')
    with path.open('rb') as f:
        while chunk := f.read(8192):
            h.update(chunk)
    b16sha = h.hexdigest()

    target_path = f'{b16sha}/{path.name}'

    extra_args = {}
    if ct: extra_args['ContentType'] = ct
    if ce: extra_args['ContentEncoding'] = ce

    log.debug("Uploading %s (%s) to %s", path, extra_args, target_path)
    bucket.upload_file(str(path), target_path, ExtraArgs=extra_args)
    return target_path


if __name__ == '__main__':
    main()
