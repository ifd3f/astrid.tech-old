import { Node } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"

export type MarkdownRemark = Node & {
  html: string
  excerpt: string
}

export type Tagged = Node & {
  tags: Tag[]
}

export type Tag = Node & {
  name: string
  color: string
  backgroundColor: string
  slug: string
  tagged: Tagged[]
}

export type WorkExperience = Tagged & {
  organization: string
  position: string
  location: string
  website: string
  startDate: Date
  endDate?: Date
  highlights: string[]
  summary?: string
}

export type Project = Tagged & {
  title: string
  status: null | "wip" | "complete" | "scrapped"
  startDate: string
  endDate: string | null
  slug: string
  url: string
  source: string[]
  thumbnail: FileSystemNode
  markdown: MarkdownRemark
  internal: {
    content: string
    description: string
  }
}

export type BlogPost = Tagged & {
  title: string
  date: Date
  slug: string

  source: MarkdownRemark
  internal: {
    description: string
  }
}

export type Course = Tagged & {
  name: string
  number: string
  slug: string
  date: string
  desc: string | null
}

export type Education = Node & {
  name: string
  degree: string | null
  startDate: string
  endDate: string
  slug: string
  courses: Course[]
}

export type SkillGroup = Node & {
  name: string
  skills: {
    level: number
    tag: Tag
  }[]
}
