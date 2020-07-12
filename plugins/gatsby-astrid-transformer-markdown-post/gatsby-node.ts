import { GatsbyNode, Node, CreateResolversArgs, SourceNodesArgs } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import { v4 } from "uuid"
import {
  BLOG_POST_MIME_TYPE,
  BlogPostContent,
} from "../gatsby-astrid-plugin-blog"
import { withContentDigest } from "../util"

type MarkdownNode = Node & {
  frontmatter: BlogMetadata
  excerpt: string
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
}) => {
  if (node.internal.type != "MarkdownRemark") return
  const markdownNode = (node as unknown) as MarkdownNode

  const parentFileSystem = getNode(markdownNode.parent) as FileSystemNode
  if (parentFileSystem.sourceInstanceName != "blog") return

  const { createNode, createParentChildLink } = actions
  const slug = createFilePath({ node, getNode })

  const content: BlogPostContent = {
    slug,
    title: markdownNode.frontmatter.title,
    date: markdownNode.frontmatter.date,
    description: markdownNode.frontmatter.description,
    markdownNode: markdownNode.id,
    tagSlugs: markdownNode.frontmatter.tags,
  }

  const postNode = withContentDigest({
    parent: markdownNode.id,
    internal: {
      type: `MarkdownBlogPost`,
      content: JSON.stringify(content),
      mediaType: BLOG_POST_MIME_TYPE,
    } as any,
    id: v4(),
    children: [],
  })

  createNode(postNode)
  createParentChildLink({ parent: markdownNode, child: postNode })
}
