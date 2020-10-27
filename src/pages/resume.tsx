import { graphql, PageProps } from "gatsby"
import React, { useState } from "react"
import { Container, Form, FormGroup, Input, Label } from "reactstrap"
import Layout from "src/components/layout"
import { TextResumeGenerator } from "src/components/resume-gen/text-field-gen"
import { Resume, ResumeGenerator } from "src/components/resume-gen/types"
import SEO from "src/components/seo"
import { Education, WorkExperience } from "src/types"
import { generateJSONResume } from "../components/resume-gen/json"
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

function ResumeGenerationView({
  generators,
  resume,
}: {
  resume: Resume
  generators: ResumeGenerator[]
}) {
  const [generatorIndex, setGeneratorIndex] = useState(0)

  const generator = generators[generatorIndex]

  return (
    <Form>
      <FormGroup row>
        <Label>Type</Label>
        <Input
          type="select"
          onChange={ev => setGeneratorIndex(parseInt(ev.target.value))}
        >
          {generators.map((g, i) => (
            <option value={i}>{g.label}</option>
          ))}
        </Input>
      </FormGroup>
      {generator.generate(resume, "wsp")}
    </Form>
  )
}

export default ({ data }: PageProps<Query>) => {
  const resume: Resume = {
    phone: "(650) 483-0527",
    email: "astrid@astrid.tech",
    projects: [],
    schools: [data.calpoly],
    work: [data.microvu, data.fabtime, data.ironPanthers],
  }

  const generators = [
    new TextResumeGenerator("md", "Markdown", generateMarkdownResume),
    new TextResumeGenerator("json", "JSON", generateJSONResume),
  ]

  return (
    <Layout>
      <SEO title="Resume" description="My Resume" />
      <main>
        <Container>
          <h1>Resume Generator</h1>
          <ResumeGenerationView resume={resume} generators={generators} />
        </Container>
      </main>
    </Layout>
  )
}
