import { GatsbyNode, Node, Actions } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"
import { BlogMetadata } from "plugins/gatsby-astrid-transformer-markdown-post"
import { v4 } from "uuid"
import { Notebook, CodeCell, Output, Cell } from "../../src/types/nbformat-v4"
import { buildNode } from "../util"
import path from "path"
import crypto from "crypto"
import { promises as fs } from "fs"

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
    private readonly actions: Actions,
    private readonly jupyterNode: JupyterNotebookNode
  ) {
    this.frontmatter = jupyterNode.data.metadata.blog_data
    this.parsedPath = path.parse(this.jupyterNode.fileAbsolutePath)
  }

  private async createImage(data: string) {
    const buf = Buffer.from(data, "base64")
    const filename = `${this.nextImage++}.generated.png`
    const outpath = path.join(`${this.parsedPath.dir}/${filename}`)
    await fs.writeFile(outpath, buf)
    return filename
  }

  private async convertOutputsToMarkdown(output: Output) {
    switch (output.output_type) {
      case "stream":
        return "```\n" + output.text + "\n```"
      case "display_data":
        const path = await this.createImage(output.data["image/png"] as string)
        const alt = output.data["text/plain"]
        return `![${alt}](./${path})`
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

  async createMarkdownFile() {
    const hashFilePath = path.join(`${this.parsedPath.dir}/hash.generated.txt`)
    const hash = crypto
      .createHash("md5")
      .update(
        this.jupyterNode.internal.contentDigest +
          (await fs.readFile(`${__dirname}/gatsby-node.ts`))
      )
      .digest()
      .toString("base64")

    if (!(await this.hasFileBeenUpdated(hashFilePath, hash))) return

    const markdownOutPath = path.join(
      `${this.parsedPath.dir}/${this.parsedPath.name}.generated.md`
    )
    await fs.writeFile(markdownOutPath, await this.createMarkdown())
    await fs.writeFile(hashFilePath, hash)
  }
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

  await new Context(actions, jupyterNode).createMarkdownFile()
}
