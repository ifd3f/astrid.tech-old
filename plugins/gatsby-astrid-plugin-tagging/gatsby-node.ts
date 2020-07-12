import {
  CreateResolversArgs,
  CreateSchemaCustomizationArgs,
  GatsbyNode,
  Node,
  SourceNodesArgs,
  BuildArgs,
  ParentSpanPluginArgs,
  CreateNodeArgs,
} from "gatsby"
import { buildTagNode, TAG_MIME_TYPE } from "./index"

export const onPreBootstrap: GatsbyNode["onPreBootstrap"] = async ({
  actions,
  schema,
}: ParentSpanPluginArgs) => {
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

export const createSchemaCustomization: GatsbyNode["createSchemaCustomization"] = async ({
  actions,
  schema,
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

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
  getNodesByType,
}: CreateNodeArgs) => {
  const { createNode, createParentChildLink } = actions

  // A taggable type
  if (Array.isArray(node.tagSlugs)) {
    node.tagSlugs.forEach(slug => {
      // Create default tags
      const tagNode = buildTagNode(
        {
          name: slug,
          slug,
          color: "#ffffff",
          backgroundColor: "#313549",
          priority: 0, // Minimum priority
        },
        node.id
      )
      createNode(tagNode)
    })
  }

  // A tag content holder
  if (node.internal.mediaType == TAG_MIME_TYPE) {
    const tagNode = buildTagNode(
      { ...JSON.parse(node.internal.content!!), priority: 1 },
      node.id
    )
    createNode(tagNode)
    createParentChildLink({ parent: node, child: tagNode as Node })
  }
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
