import path from "path"
import {
  CreateNodeArgs,
  CreateResolversArgs,
  CreateSchemaCustomizationArgs,
  GatsbyNode,
  Node,
  NodeInput,
  ParentSpanPluginArgs,
  SourceNodesArgs,
} from "gatsby"
import { v4 } from "uuid"
import { TagNodeData, TAG_MIME_TYPE } from "./index"
import { buildNode } from "../util"

function getTagID(slug: string) {
  return "tech.astrid.tagging." + slug
}

function buildTagNode(tag: TagNodeData, parent?: Node) {
  return buildNode(
    {
      internal: {
        type: `Tag`,
      },
      name: tag.name,
      slug: tag.slug,
      color: tag.color,
      priority: tag.priority,
      backgroundColor: tag.backgroundColor,
    },

    {
      parent: parent?.id,
    }
  )
}

function buildDefaultTagNode(slug: string): TagNodeData {
  return {
    name: slug,
    slug,
    color: "#ffffff",
    backgroundColor: "#313549",
    priority: 0, // Minimum priority
  }
}

const getTagNodeCreator = ({
  actions,
  getNodesByType,
}: ParentSpanPluginArgs) => (tag: TagNodeData, parent?: Node) => {
  const { createNode, createNodeField, createParentChildLink } = actions

  // Check for existing
  const existing = (getNodesByType("Tag") as (Node & TagNodeData)[]).find(
    node => node.slug == tag.slug
  )

  // Delete existing if necessary, or quit if it has higher priority
  if (existing) {
    if (existing.priority >= tag.priority) return null
    createNodeField({ node: existing, name: "valid", value: false })
  }

  // Create the node
  const tagNode = buildTagNode(tag, parent)
  createNode(tagNode)
  createNodeField({ node: tagNode as Node, name: "valid", value: true })
  if (parent) {
    createParentChildLink({ parent, child: tagNode as Node })
  }

  return tagNode as Node | NodeInput
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
  schema,
}: SourceNodesArgs) => {
  const { createTypes } = actions

  const Tagged = schema.buildInterfaceType({
    name: "Tagged",
    fields: {
      tagSlugs: "[String!]",
      tags: "[Tag!]",
    },
    extensions: { nodeInterface: {} },
  })

  const Tag = schema.buildObjectType({
    name: "Tag",
    fields: {
      id: "String!",
      name: "String!",
      slug: "String!",
      color: "String!",
      backgroundColor: "String!",
      priority: "Int!",
    },
    interfaces: ["Node"],
  })

  createTypes([Tag, Tagged])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async (
  args: CreateNodeArgs
) => {
  const { node, actions, getNodesByType } = args
  const { createNode, createParentChildLink } = actions
  const createTagNode = getTagNodeCreator(args)

  // A taggable type
  if (Array.isArray(node.tagSlugs)) {
    node.tagSlugs.forEach(slug => {
      // Create default tags
      createTagNode(buildDefaultTagNode(slug))
    })
  }

  // A tag content holder
  if (node.internal.mediaType == TAG_MIME_TYPE) {
    const tagData = JSON.parse(node.internal.content!!) as TagNodeData
    const tagNode = createTagNode(
      {
        name: tagData.name,
        color: tagData.color,
        backgroundColor: tagData.backgroundColor,
        slug: tagData.slug,
        priority: 1, // Increased priority
      },
      node
    )

    // If this slug is occupied by a higher priority tag, there are no children to link
    if (!tagNode) {
      return
    }
  }
}

function buildTagQueryForSlug(slug: string) {
  return {
    query: {
      filter: {
        slug: {
          eq: slug,
        },
        fields: {
          valid: {
            eq: true,
          },
        },
      },
    },
    type: "Tag",
    firstOnly: true,
  }
}

export const createSchemaCustomization: GatsbyNode["createSchemaCustomization"] = async ({
  actions,
  getNode,
}: CreateSchemaCustomizationArgs) => {
  const { createFieldExtension } = actions

  createFieldExtension({
    name: `tagify`,
    extend: () => ({
      resolve: async (source: any, args: any, context: any, info: any) => {
        const tags = await Promise.all(
          source.tagSlugs.map(async (slug: string) => {
            const found = await context.nodeModel.runQuery(
              buildTagQueryForSlug(slug)
            )
            if (found == null) {
              return buildDefaultTagNode(slug)
            }
            return found
          })
        )
        return tags
      },
    }),
  })

  createFieldExtension({
    name: `tagOf`,
    args: {
      fieldName: "String!",
    },
    extend: () => ({
      resolve: (source: any, args: any, context: any, info: any) =>
        (context.nodeModel.getAllNodes({ type: "Tag" }) as (Node &
          TagNodeData)[]).find(node => node.slug == source.tagSlug),
    }),
  })
}

export const createResolvers: GatsbyNode["createResolvers"] = async ({
  actions,
  createResolvers,
}: CreateResolversArgs) => {
  createResolvers({
    Tag: {
      tagged: {
        type: ["Tagged"],
        resolve(source: any, args: any, context: any, info: any) {
          return context.nodeModel
            .getAllNodes({ type: "Tagged" })
            .filter((node: any) => node.tagSlugs.includes(source.slug))
        },
      },
    },
  })
}

export const createPages: GatsbyNode["createPages"] = async ({
  actions,
  graphql,
}) => {
  const { createPage, deleteNode } = actions

  type Data = {
    allTag: {
      edges: {
        node: {
          slug: string
          tagged: any[]
          priority: number
        }
      }[]
    }
  }

  const result = await graphql(`
    {
      allTag(filter: { fields: { valid: { eq: true } } }) {
        edges {
          node {
            slug
            priority
            tagged {
              __typename
            }
          }
        }
      }
    }
  `)

  if (result.errors) {
    throw result.errors
  }

  const tags = (result.data as Data).allTag.edges

  const TagTemplate = path.resolve(`src/templates/tag.tsx`)
  // TODO reenable this

  // tags
  //   .filter(({ node }) => node.tagged.length > 0 && node.slug[0] != "/")
  //   .forEach(({ node: { slug, priority } }) => {
  //     createPage({
  //       path: "/tags/" + slug,
  //       component: TagTemplate,
  //       context: {
  //         slug: slug,
  //       },
  //     })
  //   })
}
