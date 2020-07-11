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
    },
    interfaces: ["Tagged", "Node"],
  })

  createTypes([BlogPost])
}
