export interface WorkExperience {
  organization: string
  position: string
  location: string
  website: string
  startDate: string
  endDate?: string
  highlights: string[]
  tags: string[]
  summary?: string
}

export interface Project {
  title: string
  status: null | "wip" | "complete" | "scrapped"
  description: string
  startDate: Date
  endDate: Date | null
  tags: string[]
  url: string
  source: string[]
  thumbnailURL?: string
}

export interface BlogPost {
  title: string
  date: Date
  description: string
  tags: string[]
  content: string
}

export interface Tag {
  name: string
  color: string
  textColor: string
  slug: string
}
