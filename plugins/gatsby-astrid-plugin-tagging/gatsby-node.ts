import {
  GatsbyNode,
  SourceNodesArgs,
  NodeInput,
  Node,
  CreateResolversArgs,
  CreateSchemaCustomizationArgs,
} from "gatsby"
import { withContentDigest } from "../util"
import { v4 } from "uuid"

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

  const TagResolver = schema.buildObjectType({
    name: "TagResolver",
    fields: {},
    interfaces: ["Node"],
  })

  createTypes([Tag, TagResolver, Tagged])

  createNode({
    internal: {
      type: `Tag`,
      contentDigest: "asdf",
    },
    children: [],

    id: "safghjkashjkld",
    name: "foobar",
    slug: "absc",
    backgroundColor: "#ffffff",
    color: "#ffffff",
  })
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
      const resolverNode = withContentDigest({
        parent: node.id,
        internal: { type: "TagResolver" } as any,
        children: [],
        id: v4(),
        slug,
      })
      createNode(resolverNode)
      createParentChildLink({ parent: node, child: resolverNode })

      const tagNode = buildTagNode(
        {
          name: slug,
          slug,
          color: "#ffffff",
          backgroundColor: "#313549",
          priority: 0,
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
  const tagResolverResolver = {
    TagResolver: {
      tag: {
        type: "Tag",
        resolve(source: any, args: any, context: any, info: any) {
          return context.nodeModel.runQuery({
            query: {
              filter: {
                slug: {
                  eq: source.slug,
                },
              },
              sort: { fields: ["priority"], order: ["DESC"] },
            },
            type: "Tag",
            firstOnly: true,
          })
        },
      },
    },
  }

  const taggedItemResolver = {}

  createResolvers(tagResolverResolver)
}
