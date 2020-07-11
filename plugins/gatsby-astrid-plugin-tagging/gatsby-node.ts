import {
  CreateResolversArgs,
  CreateSchemaCustomizationArgs,
  GatsbyNode,
  Node,
  NodeInput,
  SourceNodesArgs,
} from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"

type Tag = {
  name: string
  slug: string
  color: string
  backgroundColor: string
  priority: number
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  schema,
  actions,
}: SourceNodesArgs) => {
  const { createTypes, createNode } = actions

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

function buildTagNode(tag: Tag, parent?: string): NodeInput | Node {
  return withContentDigest({
    parent,
    internal: {
      type: `Tag`,
    } as any,
    children: [],

    id: v4(),
    name: tag.name,
    slug: tag.slug,
    color: tag.color,
    backgroundColor: tag.backgroundColor,
  })
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
  getNodesByType,
}) => {
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
      createParentChildLink({ parent: node, child: tagNode as Node })
    })
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
          return source.tagSlugs.map((slug: string) =>
            context.nodeModel.runQuery({
              query: {
                filter: {
                  slug: {
                    eq: slug,
                  },
                },
                distinct: "slug",
                sort: { fields: ["priority"], order: ["DESC"] },
              },
              type: "Tag",
              firstOnly: true,
            })
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
