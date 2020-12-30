const smartypants = require("@silvenon/remark-smartypants");
const remark = require("remark");
const graphviz = require("remark-graphviz");
const html = require("remark-html");
const math = require("remark-math");
const prism = require("remark-prism");

export async function renderMarkdown(md: string) {
  return (
    await remark()
      .use(prism)
      .use(math)
      .use(smartypants)
      .use(html)
      .use(graphviz)
      .process(md)
  ).toString() as string;
}
