import fs from "fs";
import { join } from "path";
import rehypeHighlight from "rehype-highlight";
import raw from "rehype-raw";
import html from "rehype-stringify";
import remark from "remark";
import emoji from "remark-emoji";
import footnotes from "remark-footnotes";
import gfm from "remark-gfm";
import math from "remark-math";
import markdown from "remark-parse";
import remark2rehype from "remark-rehype";
import slug from "remark-slug";
import unwrapImages from "remark-unwrap-images";
import { unified } from "unified";
import { VFile } from "vfile";
import { truncateKeepWords } from "./util";

// These have broken d.ts
const katex = require("rehype-katex");
const toPlainText = require("remark-plain-text");

// These do not have a d.ts
const graphviz = require("remark-graphviz");
const picture = require("rehype-picture");
const oembed = require("remark-oembed");
const urls = require("rehype-urls");
const excerpt = require("remark-excerpt");

const publicRoot = join(process.cwd(), "public");

export async function renderMarkdown(md: string, assetRoot: string) {
  function convertRelativeFileRef(url: URL) {
    if (
      url.hostname == null &&
      url.pathname != null &&
      url.pathname[0] != "/"
    ) {
      const newPath = join("/_", assetRoot, url.pathname);
      const expectedFile = join(publicRoot, newPath);
      if (fs.existsSync(expectedFile) && fs.statSync(expectedFile).isFile()) {
        return newPath;
      }
    }
    return url;
  }

  const vfile = new VFile(md);
  vfile.data = { destinationDir: join("./public/_", assetRoot) };

  const out = await unified()
    .use(markdown)
    .use(graphviz)
    .use(slug)
    .use(oembed)
    .use(emoji)
    .use(gfm)
    .use(unwrapImages)
    .use(math)
    .use(footnotes)
    .use(remark2rehype, { allowDangerousHtml: true })
    .use(raw)
    .use(rehypeHighlight)
    .use(urls, convertRelativeFileRef)
    .use(katex, { throwOnError: true })
    .use(picture)
    .use(html as any)
    .process(vfile);

  return out.toString() as string;
}

export async function getMarkdownExcerpt(md: string, maxChars: number) {
  const text = (
    await remark().use(excerpt).use(toPlainText).process(md)
  ).toString() as string;
  const result = truncateKeepWords(text, maxChars);
  if (result.neededTruncation) {
    return result.truncated + "\u2026"; // ellipsis
  }
  return result.truncated;
}

export function withoutContent<T>(object: { content: string } & T) {
  const out = {
    ...object,
  } as { content?: string } & T;
  delete out.content;
  return out as { content: never } & T;
}

export const excerptify =
  (maxChars: number) =>
  async <T>(
    object: { content: string } & T
  ): Promise<{ excerpt?: string } & T> =>
    withoutContent({
      ...object,
      excerpt: await getMarkdownExcerpt(object.content, maxChars),
    });
