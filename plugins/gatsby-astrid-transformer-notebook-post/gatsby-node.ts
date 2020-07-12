import { GatsbyNode, Node } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import { Notebook } from "src/types/nbformat-v4"
import { v4 } from "uuid"
import { BlogPostContent } from "../gatsby-astrid-plugin-blog"
import { BLOG_POST_MIME_TYPE } from "../gatsby-astrid-plugin-blog/index"
import { withContentDigest } from "../util"
import { BlogMetadata } from "plugins/gatsby-astrid-transformer-markdown-post"

type JupyterNotebookNode = Node & {
  json: Notebook
  internal: {
    content: string
  }
  fileAbsolutePath: string
  data: {
    metadata: {
      blog_data: BlogMetadata
    }
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

  const { createNode, createParentChildLink } = actions

  const id = v4()

  const frontmatter = jupyterNode.data.metadata.blog_data
  const content = jupyterNode.internal.content

  const markdown = `---
${JSON.stringify(frontmatter)}
---
${content}
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
