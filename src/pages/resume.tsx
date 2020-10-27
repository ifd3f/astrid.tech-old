import { graphql, PageProps } from "gatsby"
import { ReactNode } from "react"
import { Education, Project, WorkExperience } from "../types"

type UserInjected = {
  phone: string
  address?: string
  order?: string
}

type ResumeURL = {
  label: string
  url: string
}

type Resume = {
  email: string
  phone?: string
  address?: string
  order?: string
  urls: ResumeURL[]
  skills: { name: string; stars: number }[]
  schools: Education[]
  work: WorkExperience[]
  projects: Project[]
}

const DEFAULT_ORDER = "sewp"

export type ResumeGenerator = {
  id: string
  label: string
  generate: (resume: Resume) => ReactNode
}

type Query = {}

export const pageQuery = graphql`
  fragment GeneratorWork on Work {
    startDate(formatString: "YYYY-MM")
    endDate(formatString: "YYYY-MM")
    highlights
    location
    organization
    position
    tags {
      ...TagBadge
    }
    summary
    website
    slug
  }
  fragment GeneratorSchool on School {
    name
  }
  fragment GeneratorSkillGroup on SkillGroup {
    skills {
      level
      tag {
        name
      }
    }
  }
  fragment TextProject on Project {
    name
  }
  query ResumeGeneratorQuery {
    microvu: work(slug: { eq: "/work/micro-vu" }) {
      ...TextWork
    }
    fabtime: work(slug: { eq: "/work/fabtime" }) {
      ...TextWork
    }
    ironPanthers: work(slug: { eq: "/work/iron-panthers" }) {
      ...TextWork
    }
  }
`

export default ({ data }: PageProps<Query>) => {
  const resume: Resume = {}
}
