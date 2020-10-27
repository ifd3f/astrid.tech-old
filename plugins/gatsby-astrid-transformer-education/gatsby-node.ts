import { Actions, GatsbyNode, Node, SourceNodesArgs } from "gatsby"
import { TAG_MIME_TYPE } from "../gatsby-astrid-plugin-tagging/index"
import { buildNode } from "../util/index"

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
    backgroundColor: "#18b21b",
    color: "#ffffff",
  }

  const tagNode = buildNode(
    {
      internal: {
        type: "CourseTag",
        mediaType: TAG_MIME_TYPE,
        content: JSON.stringify(content),
      },
      children: [],
    },
    {
      parent: courseNode.id,
    }
  )

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

  const courseNode = buildNode(
    {
      internal: {
        type: "Course",
      },
      children: [],

      name: courseYamlData.name,
      slug: slug,
      number: courseYamlData.number,
      date: courseYamlData.date,
      description: courseYamlData.description,
      tagSlugs: courseYamlData.tags,
    },
    {
      parent: schoolYamlNode.id,
    }
  )

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

  const schoolNode = buildNode(
    {
      internal: {
        type: "School",
      },

      name: yamlNode.name,
      degree: yamlNode.degree,
      gpa: yamlNode.gpa,
      slug: slug,
      startDate: yamlNode.startDate,
      endDate: yamlNode.endDate,
      courses___NODE: courseNodes.map((classNode: any) => classNode.id),
    },
    {
      parent: yamlNode.id,
    }
  )
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
