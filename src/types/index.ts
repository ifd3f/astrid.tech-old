export interface WorkExperience {
  organization: string
  position: string
  location: string
  website: string
  startDate: Date
  endDate?: Date
  highlights: string[]
  tags: TagWrapper[]
  summary?: string
}

export interface Project {
  title: string
  status: null | "wip" | "complete" | "scrapped"
  description: string
  startDate: string
  endDate: string | null
  tags: TagWrapper[]
  slug: string
  url: string
  source: string[]
  markdown?: MarkdownData
  thumbnailPublicPath?: string
}

export interface BlogPost<
  ParentType = any,
  ContentTypeIndicator = "markdown" | "mdx" | "jupyter"
> {
  title?: string
  date?: Date
  description?: string
  tags?: TagWrapper[]
  slug?: string
  parent?: ParentType

  contentType?: ContentTypeIndicator
}

export type MarkdownData = {
  html?: string
  excerpt?: string
  timeToRead?: number
  wordCount?: {
    paragraphs?: number
    sentences?: number
    word?: number
  }
}

export type JupyterData = {
  internal: {
    content: string
  }
}

export type MarkdownBlogPost = BlogPost<MarkdownData, "markdown">
export type JupyterBlogPost = BlogPost<JupyterData, "jupyter">

export interface TagWrapper {
  slug?: string
  tag?: Tag
}

export interface Tag {
  name: string
  color: string
  textColor: string
  slug: string
}

export interface Course {
  name: string
  number: string
  slug: string
  date: string
  desc: string | null
  tags: TagWrapper[]
}

export interface Education {
  name: string
  degree: string | null
  startDate: string
  endDate: string
  slug: string
  courses: Course[]
}

export type SkillCategory = {
  name: string
  skills: {
    level: number
    tag: Tag
  }[]
}
