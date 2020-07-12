import { Actions, GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import { v4 } from "uuid"
import { withContentDigest } from "../util"
import { TAG_MIME_TYPE } from "../gatsby-astrid-plugin-tagging/index"

type CourseYamlData = Node & {
  name: string
  number: string
  description: string
  date: string
  slug?: string
  tags: string[]
}

type SchoolYamlNode = Node & {
  slug: string
  name: string
  degree: string
  startDate: string
  endDate: string
  courses: CourseYamlData[]
}

type CourseNode = Node & {
  name: string
  number: string
  description: string
  date: string
  slug: string
}

type CreateCourseTagNodeArgs = {
  actions: Actions
  courseNode: CourseNode
}

function createCourseTagNode({ actions, courseNode }: CreateCourseTagNodeArgs) {
  const { createNode, createParentChildLink } = actions

  const content = {
    name: courseNode.number,
    slug: courseNode.slug,
    color: "#18b21b",
    textColor: "#ffffff",
  }

  const tagNode = withContentDigest({
    parent: courseNode.id,
    id: v4(),
    internal: {
      type: "CourseTag",
      mediaType: TAG_MIME_TYPE,
      content: JSON.stringify(content),
    },
    children: [],
  })

  createNode(tagNode)
  createParentChildLink({ parent: courseNode, child: tagNode as any })

  return tagNode
}

type CreateCourseNodeArgs = {
  actions: Actions
  schoolSlug: string
  schoolYamlNode: SchoolYamlNode
  courseYamlData: CourseYamlData
}

function createCourseNode({
  actions,
  schoolSlug,
  schoolYamlNode,
  courseYamlData,
}: CreateCourseNodeArgs) {
  const { createNode, createParentChildLink } = actions

  const slug =
    schoolSlug + courseYamlData.number.replace(" ", "-").toLowerCase() + "/"

  const courseNode = withContentDigest({
    parent: schoolYamlNode.id,
    internal: {
      type: "Course",
    },
    id: v4(),
    children: [],

    name: courseYamlData.name,
    slug: slug,
    number: courseYamlData.number,
    date: courseYamlData.date,
    description: courseYamlData.description,
    tagSlugs: courseYamlData.tags,
  })

  createNode(courseNode)
  createParentChildLink({
    parent: schoolYamlNode,
    child: (courseNode as unknown) as Node,
  })

  createCourseTagNode({
    courseNode: (courseNode as unknown) as CourseNode,
    actions,
  })

  return courseNode
}

type CreateSchoolNodeArgs = {
  actions: Actions
  yamlNode: SchoolYamlNode
}

function createSchoolNode({ actions, yamlNode }: CreateSchoolNodeArgs) {
  const { createNode } = actions

  const slug = "/education/" + yamlNode.slug + "/"

  const courseNodes = yamlNode.courses.map(courseYamlData =>
    createCourseNode({
      actions,
      schoolYamlNode: yamlNode,
      schoolSlug: slug,
      courseYamlData,
    })
  )

  const schoolNode = withContentDigest({
    parent: yamlNode.id,
    internal: {
      type: "School",
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
  createNode(schoolNode)

  return schoolNode
}

export const sourceNodes: GatsbyNode["sourceNodes"] = async ({
  actions,
  schema,
}: SourceNodesArgs) => {
  const { createTypes } = actions

  const Course = schema.buildObjectType({
    name: "Course",
    fields: {
      name: "String!",
      date: "Date!",
      slug: "String!",
      number: "String!",
      description: "String",
      tagSlugs: "[String!]",
      tags: { type: "[Tag!]", extensions: { tagify: {} } },
    },
    interfaces: ["Node", "Tagged"],
  })

  const School = schema.buildObjectType({
    name: "School",
    fields: {
      name: "String!",
      degree: "String!",
      slug: "String!",
      startDate: "Date!",
      endDate: "Date!",
      courses: "[Course!]",
    },
    interfaces: ["Node"],
  })

  createTypes([Course, School])
}

export const onCreateNode: GatsbyNode["onCreateNode"] = async ({
  node,
  actions,
}) => {
  if (node.internal.type != "SchoolYaml") return
  const yamlNode = (node as unknown) as SchoolYamlNode

  createSchoolNode({ actions, yamlNode })
}
