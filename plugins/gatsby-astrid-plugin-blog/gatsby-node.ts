import { GatsbyNode, SourceNodesArgs, CreateNodeArgs } from "gatsby"
import { BLOG_POST_MIME_TYPE, BlogPostContent } from "./index"
import { withContentDigest } from "../util/index"
import { v4 } from "uuid"

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
  schema,
}: SourceNodesArgs) => {
  const { createTypes } = actions

  const BlogPost = schema.buildObjectType({
    name: "BlogPost",
    fields: {
      id: "String!",
      title: "String!",
      date: "Date!",
      slug: "String!",
      tagSlugs: "[String!]",
      tags: { type: "[Tag]", extensions: { tagify: {} } },
    },
    interfaces: ["Tagged", "Node"],
  })

  createTypes([BlogPost])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
}: CreateNodeArgs) => {
  if (node.internal.mediaType != BLOG_POST_MIME_TYPE) return

  const { createNode } = actions
  const content = JSON.parse(node.internal.content!!) as BlogPostContent

  createNode(
    withContentDigest({
      id: v4(),
      internal: {
        type: "BlogPost",
        content: content.content,
        description: content.description,
      },
      title: content.title,
      slug: content.slug,
      date: content.date,
      tagSlugs: content.tagSlugs,
    })
  )
}
