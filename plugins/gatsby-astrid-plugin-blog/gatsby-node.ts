import { CreateNodeArgs, GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import path from "path"
import { BlogMetadata, buildNode } from "../util"

type MarkdownNode = Node & {
  frontmatter: BlogMetadata
  excerpt: string
  html: string
}

function getRootFileSystemNode(
  node: Node,
  getNode: (id: string) => Node
): FileSystemNode | null {
  var out = node
  while (out) {
    if (out.internal.type == "File" && out.parent == null) {
      return out as FileSystemNode
    }
    out = getNode(out.parent)
  }
  return out as FileSystemNode
}

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
      description: "String!",
      date: "Date!",
      slug: "String!",
      tagSlugs: "[String!]",
      tags: { type: "[Tag!]", extensions: { tagify: {} } },
      source: "MarkdownRemark!",
    },
    interfaces: ["Tagged", "Node"],
  })

  createTypes([BlogPost])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
  getNode,
}: CreateNodeArgs) => {
  if (node.internal.type != "MarkdownRemark") return
  const markdownNode = (node as unknown) as MarkdownNode

  const parentFileSystem = getRootFileSystemNode(markdownNode, getNode)
  if (parentFileSystem?.sourceInstanceName != "blog") return

  const { createNode, createParentChildLink } = actions
  const slug = "/blog" + createFilePath({ node: parentFileSystem, getNode })

  const description = markdownNode.frontmatter.description
  const blogPostNode = buildNode({
    internal: {
      type: "BlogPost",
      description,
    },
    title: markdownNode.frontmatter.title,
    date: markdownNode.frontmatter.date,
    description,
    slug,
    tagSlugs: markdownNode.frontmatter.tags,
    source___NODE: markdownNode.id,
  })

  createNode(blogPostNode)
  createParentChildLink({
    parent: markdownNode,
    child: (blogPostNode as unknown) as Node,
  })
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
            slug
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  const posts = (result.data as Data).allBlogPost.edges

  const BlogPostTemplate = path.resolve(`src/templates/blog-post.tsx`)
  posts.forEach(({ node }, index) => {
    const previous = index === posts.length - 1 ? null : posts[index + 1].node
    const next = index === 0 ? null : posts[index - 1].node

    createPage({
      path: node.slug,
      component: BlogPostTemplate,
      context: {
        slug: node.slug,
        previous,
        next,
      },
    })
  })
}
