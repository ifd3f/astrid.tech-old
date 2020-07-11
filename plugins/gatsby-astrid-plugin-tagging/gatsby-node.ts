import { GatsbyNode, SourceNodesArgs } from "gatsby"

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  schema,
  actions,
}: SourceNodesArgs) => {
  const { createTypes, createNode } = actions

  const Tagged = schema.buildInterfaceType({
    name: "Tagged",
    fields: {
      tagSlugs: "[String!]",
    },
  })

  const Tag = schema.buildObjectType({
    name: "Tag",
    fields: {
      id: "String!",
      name: "String!",
      slug: "String!",
      color: "String!",
      backgroundColor: "String!",
      items: "[Tagged!]",
    },
    interfaces: ["Node"],
  })

  const TagConfig = schema.buildObjectType({
    name: "TagConfig",
    fields: {
      id: "String!",
      name: "String!",
      slug: "String!",
      color: "String!",
      backgroundColor: "String!",
    },
    interfaces: ["Node"],
  })

  createTypes([Tag, TagConfig, Tagged])

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
