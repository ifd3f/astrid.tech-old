const { parseMarkdownToHTML } = require("../src/gatsby/createNotebookPost")
const fs = require("fs")

console.log(
  parseMarkdownToHTML(
    JSON.parse(
      fs.readFileSync(
        "/home/astrid/Programming/plenglin/astrid.tech/content/blog/2020-06-18-foo/index.ipynb",
        "utf8"
      )
    )
  )
)
