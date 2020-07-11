import { Actions, NodeInput, GatsbyNode } from "gatsby"
import { v4 } from "uuid"
import { buildTagNode, withContentDigest, createLinkedTagList } from "../util"

type YamlClassNode = {
  name: string
  number: string
  tags: string[]
  desc?: string
}

type ClassNodeArg = NodeInput & {
  name: string
  number: string
  tags___NODE: string[]
  desc: string | null
}

type YamlEducationNode = NodeInput & {
  name: string
  slug: string
  degree?: string
  classes: YamlClassNode[]
}

type EducationNodeArg = NodeInput & {}

function createCourseTagNode(actions: Actions, courseNode: any) {
  const { createNode, createParentChildLink } = actions
  const tagNode = buildTagNode({
    parent: courseNode.id,
    name: courseNode.number,
    slug: courseNode.slug,
    color: "#18b21b",
    textColor: "#ffffff",
  })
  console.log(tagNode.children)
  createNode(tagNode)
  createParentChildLink({ parent: courseNode, child: tagNode as any })

  return tagNode
}

function createCourseNode(
  actions: Actions,
  parentSlug: string,
  parentId: string,
  yamlNode: any
) {
  const { createNode } = actions

  const slug =
    parentSlug + yamlNode.number.replace(" ", "-").toLowerCase() + "/"

  const courseNode = withContentDigest({
    parent: parentId,
    internal: {
      type: "Course",
    },
    id: v4(),
    children: [],

    name: yamlNode.name,
    slug: slug,
    number: yamlNode.number,
    date: yamlNode.date,
    desc: yamlNode.desc ? yamlNode.desc : null,
    tags: createLinkedTagList(yamlNode.tags),
  })

  createNode(courseNode)

  return courseNode
}

function createEducationNode(actions: Actions, yamlNode: any) {
  const { createNode } = actions

  const slug = "/education/" + yamlNode.slug + "/"

  const courseNodes = yamlNode.courses.map((raw: any) =>
    createCourseNode(actions, slug, yamlNode.id, raw)
  )

  const educationNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: "Education",
    },
    children: [],
    id: v4(),

    name: yamlNode.name,
    degree: yamlNode.degree,
    slug: slug,
    startDate: yamlNode.startDate,
    endDate: yamlNode.endDate,
    courses___NODE: courseNodes.map((classNode: any) => classNode.id),
  })
  createNode(educationNode)

  return educationNode
}

export const onCreateNode: GatsbyNode["onCreateNode"] = ({
  node,
  actions,
  getNode,
}) => {
  if (node.internal.type != "EducationYaml") return

  const { createNode, createParentChildLink } = actions
  const yamlNode = (node as unknown) as YamlEducationNode

  createEducationNode(actions, yamlNode)
}
