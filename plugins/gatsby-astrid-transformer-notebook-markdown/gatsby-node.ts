import { promises as fs } from "fs"
import { GatsbyNode, Node, NodePluginArgs } from "gatsby"
import {
  createFileNodeFromBuffer,
  FileSystemNode,
} from "gatsby-source-filesystem"
import path from "path"
import { BlogMetadata } from "plugins/gatsby-astrid-transformer-markdown-post"
import { Cell, CodeCell, Notebook, Output } from "../../src/types/nbformat-v4"

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

class Context {
  private readonly frontmatter: BlogMetadata
  private readonly parsedPath: path.ParsedPath
  private nextImage: number = 0

  constructor(
    private readonly ctx: NodePluginArgs,
    private readonly jupyterNode: JupyterNotebookNode
  ) {
    this.frontmatter = jupyterNode.data.metadata.blog_data
    this.parsedPath = path.parse(this.jupyterNode.fileAbsolutePath)
  }

  private async createImage(data: string) {
    const buffer = Buffer.from(data, "base64")
    return await createFileNodeFromBuffer({
      buffer,
      store: this.ctx.store,
      cache: this.ctx.cache,
      ext: ".png",
      createNode: this.ctx.actions.createNode,
      createNodeId: this.ctx.createNodeId,
    })
  }

  private async convertOutputsToMarkdown(output: Output) {
    switch (output.output_type) {
      case "stream":
        return "```\n" + output.text + "\n```"
      case "display_data":
        const file = await this.createImage(output.data["image/png"] as string)
        const alt = output.data["text/plain"]
        return `![${alt}](${file.absolutePath})`
    }
    return ""
  }

  async convertCodeCellToMarkdown(lang: string, cell: CodeCell) {
    const code =
      "```" + lang + "\n" + this.convertSourceToString(cell.source) + "\n```"
    const outputs = await Promise.all(
      cell.outputs.map(this.convertOutputsToMarkdown.bind(this))
    )

    return code + "\n\n" + outputs.join("\n\n")
  }

  convertSourceToString(source: string | string[]) {
    return Array.isArray(source) ? source.join("") : source
  }

  async convertCellToString(cell: Cell) {
    switch (cell.cell_type) {
      case "markdown":
        return this.convertSourceToString(cell.source)
      case "code":
        return await this.convertCodeCellToMarkdown(
          this.jupyterNode.data.metadata.language_info?.name ?? "",
          cell
        )
    }
  }

  async convertNotebookToMarkdown(notebook: Notebook): Promise<string> {
    const convertedCells = await Promise.all(
      notebook.cells.map(this.convertCellToString.bind(this))
    )

    return convertedCells.join("\n\n")
  }

  async createMarkdown() {
    return `---
${JSON.stringify(this.frontmatter)}
---
${await this.convertNotebookToMarkdown(this.jupyterNode.data)}
`
  }

  async hasFileBeenUpdated(hashFilePath: string, hash: string) {
    try {
      ;(await fs.readFile(hashFilePath)).toString() == hash
    } catch (e) {
      return true
    }
  }

  async createMarkdownFile(parent: Node) {
    const buffer = Buffer.from(await this.createMarkdown())
    const fileNode = await createFileNodeFromBuffer({
      buffer,
      parentNodeId: parent.id,
      store: this.ctx.store,
      cache: this.ctx.cache,
      ext: ".md",
      createNode: this.ctx.actions.createNode,
      createNodeId: this.ctx.createNodeId,
    })
    await this.ctx.actions.createParentChildLink({ parent, child: fileNode })
  }
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async context => {
  const { node, actions, cache, getNode, loadNodeContent } = context

  if (node.internal.type != "ipynb") return
  const jupyterNode = (node as unknown) as JupyterNotebookNode

  const parentFileSystem = getNode(jupyterNode.parent) as FileSystemNode
  if (parentFileSystem.sourceInstanceName != "blog") return

  await new Context(context, jupyterNode).createMarkdownFile(node)
}
