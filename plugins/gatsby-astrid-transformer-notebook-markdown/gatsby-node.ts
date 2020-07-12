import { GatsbyNode, Node } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"
import { BlogMetadata } from "plugins/gatsby-astrid-transformer-markdown-post"
import { v4 } from "uuid"
import { Notebook, CodeCell, Output } from "../../src/types/nbformat-v4"
import { withContentDigest } from "../util"

type JupyterNotebookNode = Node & {
  internal: {
    content: string
  }
  fileAbsolutePath: string
  data: Notebook & {
    metadata: {
      blog_data: BlogMetadata
    }
  }
}

function convertOutputsToMarkdown(output: Output) {
  switch (output.output_type) {
    case "stream":
      return "```" + output.text + "```"
    case "display_data":
      return `<img src="data:image/png;base64, ${output.data["image/png"]}" alt="${output.data["text/plain"]}" />`
  }
}

function convertCodeCellToMarkdown(lang: string, cell: CodeCell) {
  const code =
    "```" + lang + "\n" + convertSourceToString(cell.source) + "\n```"
  const outputs = cell.outputs.map(convertOutputsToMarkdown)

  return code + "\n\n" + outputs.join("\n\n")
}

function convertSourceToString(source: string | string[]) {
  return Array.isArray(source) ? source.join("") : source
}

function convertNotebookToMarkdown(notebook: Notebook): string {
  const convertedCells = notebook.cells.map(cell => {
    switch (cell.cell_type) {
      case "markdown":
        return convertSourceToString(cell.source)
      case "code":
        return convertCodeCellToMarkdown(
          notebook.metadata.language_info?.name ?? "",
          cell
        )
    }
  })

  return convertedCells.join("\n\n")
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
  getNode,
  loadNodeContent,
}) => {
  if (node.internal.type != "ipynb") return
  const jupyterNode = (node as unknown) as JupyterNotebookNode

  const parentFileSystem = getNode(jupyterNode.parent) as FileSystemNode
  if (parentFileSystem.sourceInstanceName != "blog") return

  const { createNode, createParentChildLink } = actions

  const id = v4()

  const frontmatter = jupyterNode.data.metadata.blog_data

  const markdown = `---
${JSON.stringify(frontmatter)}
---
${convertNotebookToMarkdown(jupyterNode.data)}
`

  const postNode = (withContentDigest({
    parent: jupyterNode.id,
    internal: {
      type: "JupyterMarkdownBridge",
      mediaType: "text/markdown",
      content: markdown,
    },
    id,
    children: [],
    html: jupyterNode.internal.content,
  }) as unknown) as Node

  createNode(postNode)
  createParentChildLink({ parent: jupyterNode, child: postNode })
}
