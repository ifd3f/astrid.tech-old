import { Actions, Node } from "gatsby"
import { Notebook } from "./nbformat-v4.d"
import { withContentDigest, createLinkedTagList } from "./util"
import { v4 } from "uuid"
import path from "path"
type JupyterNotebookNode = Node & {
  json: Notebook
  html: string
  fileAbsolutePath: string
  metadata: {
    blog_data: BlogMetadata
  }
}

type BlogMetadata = {
  title: string
  date: string
  description: string
  tags: string[]
}

export function createJupyterBlogPostNode(
  actions: Actions,
  jupyterNode: JupyterNotebookNode
) {
  const { createNode, createParentChildLink } = actions
  const { title, date, description, tags } = jupyterNode.metadata.blog_data

  const slugBase = path.parse(path.dirname(jupyterNode.fileAbsolutePath)).name

  const postNode = withContentDigest({
    parent: jupyterNode.id,
    internal: {
      type: "BlogPost",
    },
    id: v4(),
    children: [],
    slug: `/blog/${slugBase}/`,
    title,
    date,
    description,

    contentType: "jupyter",
    markdown___NODE: null,
    mdx___NODE: null,
    jupyter___NODE: jupyterNode.id,

    tags: createLinkedTagList(tags),
  })

  createNode(postNode)
  createParentChildLink({ parent: jupyterNode, child: postNode })
}
