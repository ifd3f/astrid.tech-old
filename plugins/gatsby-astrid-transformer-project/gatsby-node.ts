import Fuse from "fuse.js"
import { GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import { createFilePath, FileSystemNode } from "gatsby-source-filesystem"
import path from "path"
import { TagNodeData, TAG_MIME_TYPE } from "../gatsby-astrid-plugin-tagging"
import { buildNode } from "../util"
import { resolveFileNode } from "../util/index"

type MarkdownNode = Node & {
  frontmatter: ProjectMetadata
  excerpt: string
}

type ProjectMetadata = {
  title: string
  status: string
  startDate: Date
  endDate: Date
  description?: string
  featured?: boolean
  url: string
  source: string[]
  tags: string[]
  highlights?: string[]
  thumbnail: string | null
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
  schema,
}: SourceNodesArgs) => {
  const { createTypes } = actions

  const Project = schema.buildObjectType({
    name: "Project",
    fields: {
      id: "String!",
      slug: "String!",
      title: "String!",
      description: "String",
      status: "String",
      featured: "Boolean",
      startDate: "Date!",
      endDate: "Date",
      url: "String",
      source: "[String]",
      thumbnail: "File",
      markdown: "MarkdownRemark!",
      highlights: "[String!]",
      tagSlugs: "[String!]",
      tags: { type: "[Tag!]", extensions: { tagify: {} } },
    },
    interfaces: ["Tagged", "Node"],
  })

  const ProjectSearchIndex = schema.buildObjectType({
    name: "ProjectSearchIndex",
    fields: {
      id: "String!",
      data: "String!",
      keys: "[String!]",
    },
    interfaces: ["Node"],
  })

  createTypes([Project, ProjectSearchIndex])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
  getNode,
  getNodesByType,
  loadNodeContent,
}) => {
  if (node.internal.type != "MarkdownRemark") return
  const markdownNode = (node as unknown) as MarkdownNode

  const parentFileSystem = getNode(markdownNode.parent) as FileSystemNode
  if (parentFileSystem.sourceInstanceName != "projects") return

  const { createNode, createParentChildLink } = actions
  const slug = "/projects" + createFilePath({ node, getNode })

  const frontmatter = markdownNode.frontmatter
  var thumbnailFileNodeId = null
  if (frontmatter.thumbnail) {
    const file = resolveFileNode({
      file: getNode(markdownNode.parent) as FileSystemNode,
      relativePath: frontmatter.thumbnail,
      getNodesByType,
    })
    thumbnailFileNodeId = file?.id ?? null
  }

  const projectNode = buildNode(
    {
      internal: {
        type: `Project`,
        description: frontmatter.description,
        mediaType: "text/html",
      } as any,

      slug: slug,
      url: frontmatter.url,
      source: frontmatter.source,
      title: frontmatter.title,
      description: frontmatter.description,
      featured: frontmatter.featured ?? false,
      status: frontmatter.status,
      startDate: frontmatter.startDate,
      highlights: frontmatter.highlights,
      endDate: frontmatter.endDate,
      thumbnail___NODE: thumbnailFileNodeId,
      markdown___NODE: markdownNode.id,
      tagSlugs: markdownNode.frontmatter.tags,
    },
    {
      parent: markdownNode.id,
    }
  )
  createNode(projectNode)
  createParentChildLink({ parent: markdownNode, child: projectNode })

  const tagContent: TagNodeData = {
    name: projectNode.title,
    slug: projectNode.slug,
    backgroundColor: "#313549", // TODO: CHANGE TO NICER COLORING SYSTEM
    color: "#ffffff",
    priority: 2,
  }

  const tagNode = buildNode(
    {
      internal: {
        type: "ProjectTag",
        mediaType: TAG_MIME_TYPE,
        content: JSON.stringify(tagContent),
      },
    },
    {
      parent: projectNode.id,
    }
  )

  createNode(tagNode)
  createParentChildLink({
    parent: projectNode,
    child: (tagNode as unknown) as Node,
  })
}

export const createPages: GatsbyNode["createPages"] = async ({
  graphql,
  actions,
}) => {
  const { createPage, createNode } = actions

  type Data = {
    allProject: {
      edges: {
        node: {
          slug: string
          internal: { description: string }
          title: string
          tags: {
            name: string
            slug: string
          }[]
        }
      }[]
    }
  }

  const result = await graphql(`
    {
      allProject(sort: { fields: [endDate], order: DESC }) {
        edges {
          node {
            slug
            internal {
              description
            }
            title
            tags {
              name
              slug
            }
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  const projects = (result.data as Data).allProject.edges
    .filter(({ node }) => {
      if (node.tags) return true
      console.warn("tags is null for queried project node", node)
      return false
    })
    .map(({ node }) => node)

  const keys = [
    "title",
    "slug",
    "internal.description",
    "tags.name",
    "tags.slug",
  ]
  const index = Fuse.createIndex(keys, projects)

  createNode(
    buildNode({
      internal: {
        type: "ProjectSearchIndex",
      },
      data: JSON.stringify(index.toJSON()),
      keys,
    })
  )

  const ProjectDetailTemplate = path.resolve(`src/templates/project-detail.tsx`)

  projects.forEach(({ slug }) => {
    createPage({
      path: slug,
      component: ProjectDetailTemplate,
      context: { slug },
    })
  })
}
