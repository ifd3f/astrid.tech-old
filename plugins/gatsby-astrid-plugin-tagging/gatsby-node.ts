import {
  CreateSchemaCustomizationArgs,
  GatsbyNode,
  SourceNodesArgs,
} from "gatsby"

export const createSchemaCustomization: GatsbyNode["createSchemaCustomization"] = async ({
  actions,
}: CreateSchemaCustomizationArgs) => {
  const { createTypes, createFieldExtension } = actions

  createTypes(`
    interface Tagged @nodeinterface {
      id: String!
      tagSlugs: [String!]
    }

    type TagConfig implements Node {
      name: String!
      slug: String! 
      backgroundColor: String!
      color: String!
    }

    type Tag implements Node {
      name: String!
      slug: String!
      backgroundColor: String!
      color: String!
      items: [Tagged!]
    }
  `)
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
}: SourceNodesArgs) => {
  const { createNode } = actions

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
