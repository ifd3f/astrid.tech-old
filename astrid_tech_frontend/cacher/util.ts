import walk from 'walk';

export async function walkArr<T>(dir: string) {
  const out: { root: string; stats: walk.WalkStats }[] = [];
  await new Promise<void>((resolve, reject) => {
    const walker = walk.walk(dir);
    walker.on('file', async (root, stats, next) => {
      out.push({ root, stats });
      next();
    });

    walker.on('errors', (root, nodeStatsArray) =>
      reject({ root, nodeStatsArray })
    );

    walker.on('end', resolve);
  });
  return out;
}

export function serializeJS(data: any, msg?: string) {
  return `/* This is an AUTO-GENERATED FILE. ${msg ?? ''} */
/* eslint-disable */
// prettier-ignore
module.exports=${JSON.stringify(data)}`;
}
