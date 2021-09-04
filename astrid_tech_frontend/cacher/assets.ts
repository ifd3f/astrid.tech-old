import { promises as fs } from 'fs';
import yaml from 'js-yaml';
import path, { join } from 'path';
import { serializeJS, walkArr } from './util';

const getDestinationDir =
  (contentDir: string, outDir: string) => async (src: string) => {
    const relative = path.relative(contentDir, src);
    const dest = relative.startsWith('blog/')
      ? join(outDir, relative.substring(5))
      : join(outDir, relative);

    await fs.mkdir(path.parse(dest).dir, {
      recursive: true,
    });
    return { src, dest };
  };

export async function copyAssets(
  contentDir: string,
  outputDir: string
): Promise<void> {
  console.log('Copying assets');
  await fs.rm(outputDir, { force: true, recursive: true });
  const resolver = getDestinationDir(contentDir, outputDir);

  const files = (await walkArr(contentDir))
    .filter(
      ({ stats }) =>
        stats.isFile() &&
        !stats.name.endsWith('.md') &&
        !stats.name.endsWith('.yaml')
    )
    .map(({ root, stats }) =>
      (async () => {
        const { src, dest } = await resolver(join(root, stats.name));
        await fs.copyFile(src, dest);
      })()
    );
  await Promise.all(files);
}

export async function mapData(
  contentDir: string,
  outputDir: string
): Promise<void> {
  console.log('Mapping data');
  await fs.rm(outputDir, { force: true, recursive: true });
  const resolver = getDestinationDir(contentDir, outputDir);

  const files = (await walkArr(contentDir))
    .filter(({ stats }) => stats.isFile() && stats.name.endsWith('.yaml'))
    .map(({ root, stats }) =>
      (async () => {
        const { src, dest } = await resolver(join(root, stats.name));
        const jsDest = dest.slice(0, -5) + '.js';
        const data = yaml.load((await fs.readFile(src)).toString());
        await fs.writeFile(
          jsDest,
          serializeJS(data, `Source file was at ${src}`)
        );
      })()
    );
  await Promise.all(files);
}
