import { GatsbyNode, Node } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import { v4 } from "uuid"
import { withContentDigest } from "../util"

type MarkdownNode = Node & {
  frontmatter: BlogMetadata
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
  if (node.internal.type != "MarkdownRemark") return
  const markdownNode = (node as unknown) as MarkdownNode

  const parentFileSystem = getNode(markdownNode.parent) as FileSystemNode
  if (parentFileSystem.sourceInstanceName != "blog") return

  const { createNode, createParentChildLink } = actions
  const slug = createFilePath({ node, getNode })

  const postNode = withContentDigest({
    parent: markdownNode.id,
    internal: {
      type: `BlogPost`,
      content: await loadNodeContent(markdownNode),
    } as any,
    id: v4(),
    children: [],

    slug: slug,
    title: markdownNode.frontmatter.title,
    date: markdownNode.frontmatter.date,
    description: markdownNode.frontmatter.description,

    tagSlugs: markdownNode.frontmatter.tags,
  })

  createNode(postNode)
  createParentChildLink({ parent: markdownNode, child: postNode })
}
