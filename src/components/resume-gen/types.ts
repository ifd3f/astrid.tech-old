import { ReactNode } from "react"
import { Education, Project, WorkExperience } from "../../types"

export type UserInjected = {
  phone: string
  address?: string
  order?: string
}

export type ResumeURL = {
  label: string
  url: string
}

export type Resume = {
  phone: string
  email: string
  address?: string
  schools: Education[]
  work: WorkExperience[]
  projects: Project[]
}

export const DEFAULT_ORDER = "wsp"
export const SECTION_WORK = "w"
export const SECTION_EDUCATION = "s"
export const SECTION_PROJECTS = "p"

export interface ResumeGenerator {
  ext: string
  label: string
  generate: (resume: Resume, order: string) => ReactNode
}
