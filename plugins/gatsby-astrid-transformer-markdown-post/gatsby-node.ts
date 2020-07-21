import { GatsbyNode, Node, CreateResolversArgs, SourceNodesArgs } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import { v4 } from "uuid"
import {
  BLOG_POST_MIME_TYPE,
  BlogPostContent,
} from "../gatsby-astrid-plugin-blog"
import { BlogMetadata } from "./index"
import { buildNode } from "../util"

type MarkdownNode = Node & {
  frontmatter: BlogMetadata
  excerpt: string
  html: string
}

function getFileSystemNode(
  node: Node,
  getNode: (id: string) => Node
): FileSystemNode | null {
  var out = node
  while (out && out.internal.type != "File") {
    if (out.parent == null) {
      return null
    }
    out = getNode(out.parent)
  }
  return out as FileSystemNode
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
  getNode,
}) => {
  if (node.internal.type != "MarkdownRemark") return
  const markdownNode = (node as unknown) as MarkdownNode

  const parentFileSystem = getFileSystemNode(markdownNode, getNode)
  if (parentFileSystem?.sourceInstanceName != "blog") return

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

  const postNode = buildNode(
    {
      internal: {
        type: `MarkdownBlogPost`,
        content: JSON.stringify(content),
        mediaType: BLOG_POST_MIME_TYPE,
      } as any,
    },
    {
      parent: markdownNode.id,
    }
  )

  createNode(postNode)
  createParentChildLink({ parent: markdownNode, child: postNode })
}
