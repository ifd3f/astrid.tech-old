const smartypants = require("@silvenon/remark-smartypants");
import { join } from "path";
const graphviz = require("remark-graphviz");
const math = require("remark-math");
const prism = require("remark-prism");
const unwrapImages = require("remark-unwrap-images");
const picture = require("rehype-picture");
const remark2rehype = require("remark-rehype");
const html = require("rehype-stringify");
const katex = require("rehype-katex");
const footnotes = require("remark-footnotes");
const oembed = require("remark-oembed");
const markdown = require("remark-parse");
const unified = require("unified");
const slug = require("remark-slug");
const urls = require("rehype-urls");

export async function renderMarkdown(md: string, assetRoot: string) {
  function convertRelative(url: URL) {
    if (url.hostname != null) return url;
    return join(assetRoot, url.pathname);
  }

  const out = await unified()
    .use(markdown)
    .use(slug)
    .use(oembed)
    .use(graphviz)
    .use(unwrapImages)
    .use(smartypants)
    .use(math)
    .use(prism)
    .use(footnotes)
    .use(remark2rehype)
    .use(urls, convertRelative)
    .use(katex)
    .use(picture)
    .use(html)
    .process(md);
  return out.contents as string;
}
