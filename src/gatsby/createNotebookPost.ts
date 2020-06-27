import { FileSystemNode } from "gatsby-source-filesystem"
import { Actions } from "gatsby"
import fs from "fs"
import { Notebook, CodeCell, MarkdownCell } from "./nbformat-v4.d"
import Remark from "remark"

export function parseMarkdownToHTML(data: string): string {
  const remark = Remark().data("settings", {
    commonmark: true,
    gfm: true,
    pedantic: true,
    footnotes: true,
  })
  console.log(remark)
  return ""
}

export function notebookToHTML(notebook: Notebook): string {
  notebook.cells.map(cell => {
    switch (cell.cell_type) {
      case "code": {
        cell as CodeCell
        cell
        break
      }
      case "markdown": {
        cell as MarkdownCell
        const contents =
          cell.source instanceof Array ? cell.source.join(",") : cell.source
        console.log(contents)
      }
    }
  })
  return ""
}

function createNotebookNode(actions: Actions, node: FileSystemNode) {
  const notebook: Notebook = JSON.parse(
    fs.readFileSync(node.absolutePath, "utf8")
  )
  const cells = []
  notebook.cells.map(cell => {
    switch (cell.cell_type) {
      case "code": {
        cell as CodeCell
        cell
        break
      }
      case "markdown": {
        cell as MarkdownCell
        const contents =
          cell.source instanceof Array ? cell.source.join(",") : cell.source
      }
    }
  })
}
