import {
  Actions,
  CreateNodeArgs,
  GatsbyNode,
  Node,
  SourceNodesArgs,
} from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"

type YamlSkillLevel = {
  slug: string
  level: number
}

type YamlSkillNode = Node & {
  name: string
  skills: YamlSkillLevel[]
}

type CreateSkillLevelArgs = {
  data: YamlSkillLevel
  actions: Actions
  yamlNode: YamlSkillNode
}

function createSkillLevel({ data, actions, yamlNode }: CreateSkillLevelArgs) {
  const { createNode, createParentChildLink } = actions
  const node = withContentDigest({
    parent: yamlNode.id,
    id: v4(),
    internal: {
      type: "SkillLevel",
    },

    tagSlug: data.slug,
    level: data.level,
  })
  createNode(node)
  createParentChildLink({ parent: yamlNode, child: (node as unknown) as Node })
  return node
}

type CreateSkillGroupArgs = {
  yamlNode: YamlSkillNode
  skillIds: string[]
  actions: Actions
}

function createSkillGroup({
  yamlNode,
  skillIds,
  actions,
}: CreateSkillGroupArgs) {
  const { createNode, createParentChildLink } = actions
  const node = withContentDigest({
    parent: yamlNode.id,
    id: v4(),
    internal: {
      type: "SkillGroup",
    },

    name: yamlNode.name,
    skills___NODE: skillIds,
  })
  createNode(node)
  createParentChildLink({ parent: yamlNode, child: (node as unknown) as Node })
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  schema,
  actions,
}: SourceNodesArgs) => {
  const { createTypes, createParentChildLink } = actions

  const SkillLevel = schema.buildObjectType({
    name: "SkillLevel",
    fields: {
      tag: { type: "Tag!", extensions: { tagOf: { fieldName: "tagSlug" } } },
      tagSlug: "String!",
      level: "Int!",
    },
    interfaces: ["Node"],
  })

  const SkillGroup = schema.buildObjectType({
    name: "SkillGroup",
    fields: {
      name: "String!",
      skills: "[SkillLevel]",
    },
    interfaces: ["Node"],
  })

  createTypes([SkillLevel, SkillGroup])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
}: CreateNodeArgs) => {
  if (node.internal.type != "SkillsYaml") return

  const yamlNode = (node as unknown) as YamlSkillNode

  const skillLevels = yamlNode.skills.map(skillData =>
    createSkillLevel({ data: skillData, actions, yamlNode })
  )

  createSkillGroup({
    yamlNode,
    skillIds: skillLevels.map(node => node.id),
    actions,
  })
}
