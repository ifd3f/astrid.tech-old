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
const raw = require("rehype-raw");
const oembed = require("remark-oembed");
const markdown = require("remark-parse");
const unified = require("unified");
const slug = require("remark-slug");
const gfm = require("remark-gfm");
const urls = require("rehype-urls");

export async function renderMarkdown(md: string, assetRoot: string) {
  function convertRelative(url: URL) {
    if (url.hostname != null) return url;
    return join("/_", assetRoot, url.pathname);
  }

  const out = await unified()
    .use(markdown)
    .use(slug)
    .use(graphviz)
    .use(oembed)
    .use(gfm)
    .use(unwrapImages)
    .use(smartypants)
    .use(math)
    .use(prism)
    .use(footnotes)
    .use(remark2rehype, { allowDangerousHtml: true })
    .use(raw)
    .use(urls, convertRelative)
    .use(katex)
    .use(picture)
    .use(html, { sanitize: false })
    .process(md);
  return out.contents as string;
}
