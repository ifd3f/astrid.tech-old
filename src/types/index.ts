import { Node, PackageJson } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"

export type MarkdownRemark = Node & {
  html: string
  excerpt: string
}

export type Tagged = TypeName & {
  tags: Tag[]
  slug: string
}

export type Tag = Node & {
  name: string
  color: string
  backgroundColor: string
  slug: string
  tagged: Tagged[]
}

export type TypeName = Node & {
  __typename: string
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
  __typename: "Project"
  title: string
  status: null | "wip" | "complete" | "scrapped"
  startDate: string
  endDate: string | null
  slug: string
  url: string
  source: string[]
  thumbnail: FileSystemNode
  markdown: MarkdownRemark
  childProjectTag: { childTag: Tag }
  internal: {
    content: string
    description: string
  }
}

export type BlogPost = Tagged & {
  __typename: "BlogPost"
  title: string
  description: string
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

export type SiteMetadata = {
  siteUrl: string
  title: string
  cookiePolicyVersion: string
  package: PackageJson & {
    bugs: {
      url: string
    }
  }
}

export type Site = Node & {
  siteMetadata: SiteMetadata
}
