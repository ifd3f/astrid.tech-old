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
  phone: string
  email: string
  address?: string
  order?: string
  urls: ResumeURL[]
  skills: { name: string; stars: number }[]
  schools: Education[]
  work: WorkExperience[]
  projects: Project[]
}

const DEFAULT_ORDER = "sewp"

type ResumeGenerator = {
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

export const TextResumeGenerator: ResumeGenerator = {
  id: "txt",
  label: "Basic Text",
  generate: resume => {
    const heading = [
      "Astrid Yu",
      `Email: ${resume.email}`,
      `Phone: ${resume.phone}`,
      resume.address ? `Address: ${resume.address}` : "",
    ]

    const skills = ["Skills"].concat(
      resume.skills.map(({ name, stars }) => `- ${name}: ${stars}`)
    )

    const education = [""]

    const work = [""]
    const projects = [""]

    const mapping: Map<string, string[]> = new Map([
      ["e", education],
      ["w", work],
      ["s", skills],
      ["p", projects],
    ])

    const order = resume.order ?? DEFAULT_ORDER
    for (let i = 0; i < order.length; i++) {
      const char = order.charAt(i)
      heading.push("\n")
      for (let line of mapping.get(char)!!) {
        heading.push(line)
      }
    }

    return heading.join("\n")
  },
}

export default ({ data }: PageProps<Query>) => {}
