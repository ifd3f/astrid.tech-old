import { promises as fs } from "fs";
import path, { join } from "path";
import { walkArr } from "./util";

export async function copyAssets(
  contentDir: string,
  outputDir: string
): Promise<void> {
  await fs.rm(outputDir, { force: true, recursive: true });

  const files = (await walkArr(contentDir))
    .filter(({ stats }) => stats.isFile() && !stats.name.endsWith(".md"))
    .map(({ root, stats }) =>
      (async () => {
        const src = join(root, stats.name);
        const relative = path.relative(contentDir, src);
        const dest = join(outputDir, relative);

        await fs.mkdir(path.parse(dest).dir, {
          recursive: true,
        });
        await fs.copyFile(src, dest);
      })()
    );
  await Promise.all(files);
}
