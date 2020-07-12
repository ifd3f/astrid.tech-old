import { CreateNodeArgs, GatsbyNode, SourceNodesArgs } from "gatsby"
import path from "path"
import { v4 } from "uuid"
import { withContentDigest } from "../util/index"
import { BlogPostContent, BLOG_POST_MIME_TYPE } from "./index"

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
      tags: { type: "[Tag!]", extensions: { tagify: {} } },
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

  const slug = "/blog" + content.slug

  createNode(
    withContentDigest({
      id: v4(),
      internal: {
        type: "BlogPost",
        content: content.content,
        description: content.description,
      },
      title: content.title,
      slug,
      date: content.date,
      tagSlugs: content.tagSlugs,
    })
  )
}

export const createPages: GatsbyNode["createPages"] = async ({
  graphql,
  actions,
}) => {
  const { createPage } = actions

  type Data = {
    allBlogPost: {
      edges: {
        node: {
          title: string
          slug: string
        }
      }[]
    }
  }

  const result = await graphql(`
    {
      allBlogPost(sort: { fields: date, order: DESC }) {
        edges {
          node {
            title
            id
            slug
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  // Create blog posts pages.
  const posts = (result.data as Data).allBlogPost.edges

  const BlogPostTemplate = path.resolve(`src/templates/blog-post.tsx`)
  posts.forEach((edge, index) => {
    const post = edge.node

    const previous = index === posts.length - 1 ? null : posts[index + 1].node
    const next = index === 0 ? null : posts[index - 1].node

    createPage({
      path: post.slug,
      component: BlogPostTemplate,
      context: {
        slug: post.slug,
        previous,
        next,
      },
    })
  })
}
