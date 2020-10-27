import { graphql, PageProps } from "gatsby"
import React from "react"
import { Container } from "reactstrap"
import Layout from "src/components/layout"
import { TextResumeGenerator } from "src/components/resume-gen/text-field-gen"
import { Resume } from "src/components/resume-gen/types"
import SEO from "src/components/seo"
import { Education, WorkExperience } from "src/types"
import { generateMarkdownResume } from "../components/resume-gen/markdown"

const DEFAULT_ORDER = "sewp"

type Query = {
  calpoly: Education
  microvu: WorkExperience
  fabtime: WorkExperience
  ironPanthers: WorkExperience
}

export const pageQuery = graphql`
  fragment GeneratorWork on Work {
    startDate(formatString: "MMM YYYY")
    endDate(formatString: "MMM YYYY")
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
    degree
    gpa
    startDate(formatString: "MMM YYYY")
    endDate(formatString: "MMM YYYY")
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
    calpoly: school(slug: { eq: "/education/cal-poly/" }) {
      ...GeneratorSchool
    }
    microvu: work(slug: { eq: "/work/micro-vu/" }) {
      ...GeneratorWork
    }
    fabtime: work(slug: { eq: "/work/fabtime/" }) {
      ...GeneratorWork
    }
    ironPanthers: work(slug: { eq: "/work/iron-panthers/" }) {
      ...GeneratorWork
    }
  }
`

export default ({ data }: PageProps<Query>) => {
  const resume: Resume = {
    phone: "(650) 483-0527",
    email: "astrid@astrid.tech",
    projects: [],
    schools: [data.calpoly],
    work: [data.microvu, data.fabtime, data.ironPanthers],
  }

  const generator = new TextResumeGenerator(
    "md",
    "Markdown",
    generateMarkdownResume
  )

  return (
    <Layout>
      <SEO title="Resume" description="My Resume" />
      <main>
        <Container>{generator.generate(resume, "wsp")}</Container>
      </main>
    </Layout>
  )
}
