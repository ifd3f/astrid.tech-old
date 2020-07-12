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
import { withContentDigest } from "../util"
import { TagNodeData, TAG_MIME_TYPE } from "./index"

const getTagNodeCreator = ({
  actions,
  getNodesByType,
}: ParentSpanPluginArgs) => (tag: TagNodeData, parent?: string) => {
  const { createNode, deleteNode } = actions

  // Check for existing
  const existing = (getNodesByType("Tag") as (Node & TagNodeData)[]).find(
    node => node.slug == tag.slug
  )

  // Delete existing if necessary, or quit if it has higher priority
  if (existing) {
    if (existing.priority >= tag.priority) return null
    deleteNode({ node: existing })
  }

  // Create the node
  const tagNode = withContentDigest({
    parent,
    internal: {
      type: `Tag`,
    } as any,
    children: [],

    id: v4(),
    name: tag.name,
    slug: tag.slug,
    color: tag.color,
    priority: tag.priority,
    backgroundColor: tag.backgroundColor,
  })
  createNode(tagNode)

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
      tagSlugs: "[String]",
      tags: "[Tag]",
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
  const buildTagNode = getTagNodeCreator(args)

  // A taggable type
  if (Array.isArray(node.tagSlugs)) {
    node.tagSlugs.forEach(slug => {
      // Create default tags
      buildTagNode(
        {
          name: slug,
          slug,
          color: "#ffffff",
          backgroundColor: "#313549",
          priority: 0, // Minimum priority
        },
        node.id
      )
    })
  }

  // A tag content holder
  if (node.internal.mediaType == TAG_MIME_TYPE) {
    const tagNode = buildTagNode(
      { ...JSON.parse(node.internal.content!!), priority: 1 }, // Increased priority
      node.id
    )
    createParentChildLink({ parent: node, child: tagNode as Node })
  }
}

export const createSchemaCustomization: GatsbyNode["createSchemaCustomization"] = async ({
  actions,
}: CreateSchemaCustomizationArgs) => {
  const { createFieldExtension } = actions

  createFieldExtension({
    name: `tagify`,
    extend: () => ({
      resolve: (source: any, args: any, context: any, info: any) =>
        source.tagSlugs.map((slug: string) =>
          context.nodeModel.runQuery({
            query: {
              filter: {
                slug: {
                  eq: slug,
                },
              },
            },
            type: "Tag",
            firstOnly: true,
          })
        ),
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
