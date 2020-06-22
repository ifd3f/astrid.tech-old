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

export interface BlogPost {
  title?: string
  date?: Date
  description?: string
  tags?: TagWrapper[]
  slug?: string

  contentType?: "markdown" | "mdx" | "jupyter"
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

export interface MarkdownBlogPost extends BlogPost {
  contentType?: "markdown"
  markdown?: MarkdownData
}

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
