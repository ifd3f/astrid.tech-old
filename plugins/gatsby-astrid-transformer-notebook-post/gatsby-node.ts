import { GatsbyNode, Node } from "gatsby"
import path from "path"
import { Notebook } from "src/types/nbformat-v4"
import { v4 } from "uuid"
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
  loadNodeContent,
}) => {
  if (node.internal.type != "ipynb") return

  const jupyterNode = (node as unknown) as JupyterNotebookNode
  const { createNode, createParentChildLink } = actions
  const { title, date, description, tags } = jupyterNode.data.metadata.blog_data

  const slugBase = path.parse(path.dirname(jupyterNode.fileAbsolutePath)).name

  const postNode = (withContentDigest({
    parent: jupyterNode.id,
    internal: {
      type: "BlogPost",
      //content: jupyterNode.internal.content,
    },
    id: v4(),
    children: [],
    slug: `/blog/${slugBase}/`,
    title,
    date,
    description,
    tagSlugs: tags,
  }) as unknown) as Node

  createNode(postNode)
  createParentChildLink({ parent: jupyterNode, child: postNode })
}
