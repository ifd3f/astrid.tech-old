import { GatsbyNode, SourceNodesArgs } from "gatsby"

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
      date: "Date!",
      description: "String!",
      tagSlugs: "[String!]",
      tags: { type: "[Tag]", extensions: { tagify: {} } },
    },
    interfaces: ["Tagged", "Node"],
  })

  createTypes([BlogPost])
}
