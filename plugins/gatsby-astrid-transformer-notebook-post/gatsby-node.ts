import { GatsbyNode, Node } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import { Notebook } from "src/types/nbformat-v4"
import { v4 } from "uuid"
import { BlogPostContent } from "../gatsby-astrid-plugin-blog"
import { BLOG_POST_MIME_TYPE } from "../gatsby-astrid-plugin-blog/index"
import { withContentDigest } from "../util"

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
  html: string
}

type BlogMetadata = {
  title: string
  date: string
  description: string
  tags: string[]
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
  const { title, date, description, tags } = jupyterNode.data.metadata.blog_data

  const slug = createFilePath({ node, getNode })

  const content: BlogPostContent = {
    slug,
    title,
    date,
    description,
    content: await loadNodeContent(jupyterNode),
    tagSlugs: tags,
  }

  const postNode = (withContentDigest({
    parent: jupyterNode.id,
    internal: {
      type: "JupyterBlogPost",
      mediaType: BLOG_POST_MIME_TYPE,
      content: JSON.stringify(content),
    },
    id: v4(),
    children: [],
  }) as unknown) as Node

  createNode(postNode)
  createParentChildLink({ parent: jupyterNode, child: postNode })
}
