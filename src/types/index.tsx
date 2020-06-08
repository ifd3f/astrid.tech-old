export interface WorkExperience {
  organization: string
  position: string
  location: string
  website: string
  startDate: string
  endDate?: string
  highlights: string[]
  skills: string[]
  summary?: string
}

export interface Project {
  title: string
  status: null | "wip" | "complete"
  desc: string
  startDate: Date
  endDate: Date | null
  skills: string[]
  url: string
  source: string
}

export interface BlogPost {
  title: string
  date: Date
  description: string
  tags: string[]
  content: string
}
