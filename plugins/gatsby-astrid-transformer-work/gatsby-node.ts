import { Actions, GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"
import { TAG_MIME_TYPE } from "../gatsby-astrid-plugin-tagging/index"

type WorkYamlNode = Node & {
  slug: string
  organization: string
  position: string
  location: string
  startDate: string
  endDate?: string
  summary: string
  website: string
  highlights: string[]
  tags: string[]
}

type CreateWorkNodeArgs = {
  actions: Actions
  yamlNode: WorkYamlNode
}

function createWorkNode({ actions, yamlNode }: CreateWorkNodeArgs) {
  const { createNode, createParentChildLink } = actions
  const workNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: `Work`,
    },
    id: v4(),
    children: [],

    slug: "/work/" + yamlNode.slug,
    organization: yamlNode.organization,
    position: yamlNode.position,
    location: yamlNode.location,
    startDate: yamlNode.startDate,
    endDate: yamlNode.endDate,

    summary: yamlNode.summary,
    website: yamlNode.website,

    highlights: yamlNode.highlights,

    tagSlugs: yamlNode.tags,
  })
  createNode(workNode)
  createParentChildLink({
    parent: yamlNode,
    child: (workNode as unknown) as Node,
  })
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
  schema,
}: SourceNodesArgs) => {
  const { createTypes } = actions

  const Work = schema.buildObjectType({
    name: "Work",
    fields: {
      slug: "String!",
      organization: "String!",
      position: "String!",
      location: "String!",
      startDate: "Date!",
      endDate: "Date",
      summary: "String!",
      website: "String!",
      highlights: "[String!]",
      tagSlugs: "[String!]",
      tags: { type: "[Tag!]", extensions: { tagify: {} } },
    },
    interfaces: ["Node", "Tagged"],
  })

  createTypes([Work])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
}) => {
  if (node.internal.type != "WorkYaml") return
  const yamlNode = (node as unknown) as WorkYamlNode

  createWorkNode({ actions, yamlNode })
}
