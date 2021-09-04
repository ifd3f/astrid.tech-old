import path from 'path';

export function resolveAssetURL(root: string, pathname: string) {
  if (pathname.startsWith('/', 0)) {
    return path.join('/_', pathname);
  }
  return path.join('/_', root, pathname);
}
