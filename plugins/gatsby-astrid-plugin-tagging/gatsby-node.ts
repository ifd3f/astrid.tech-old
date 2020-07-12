import {
  CreateNodeArgs,
  CreateResolversArgs,
  CreateSchemaCustomizationArgs,
  GatsbyNode,
  Node,
  ParentSpanPluginArgs,
  SourceNodesArgs,
  NodeInput,
} from "gatsby"
import { withContentDigest } from "../util"
import { v4 } from "uuid"
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
      { ...JSON.parse(node.internal.content!!), priority: 1 },
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
    extend() {
      return {
        resolve(source: any, args: any, context: any, info: any) {
          const allTags = context.nodeModel.getAllNodes({ type: "Tag" })
          return source.tagSlugs.map((slug: string) =>
            (allTags.filter(
              (tag: any) => tag.slug == slug
            ) as any[]).reduce((a, b) => (a.priority > b.priority ? a : b))
          )
        },
      }
    },
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
