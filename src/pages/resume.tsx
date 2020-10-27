import { graphql, PageProps } from "gatsby"
import React, { useEffect, useState } from "react"
import { useCookies } from "react-cookie"
import { Col, Container, Form, FormGroup, Input, Label, Row } from "reactstrap"
import Layout from "src/components/layout"
import { TextResumeGenerator } from "src/components/resume-gen/text-field-gen"
import {
  Resume,
  ResumeGenerator,
  UserInjected,
} from "src/components/resume-gen/types"
import SEO from "src/components/seo"
import { Education, Project, WorkExperience } from "src/types"
import { generateJSONResume } from "../components/resume-gen/json"
import { generateMarkdownResume } from "../components/resume-gen/markdown"

const DEFAULT_ORDER = "sewp"

type Query = {
  calpoly: Education
  microvu: WorkExperience
  fabtime: WorkExperience
  ironPanthers: WorkExperience
  collisionZone: Project
  astridTech: Project
  segway: Project
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
  fragment GeneratorProject on Project {
    title
    description
    highlights
    tags {
      name
    }
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
    astridTech: project(slug: { eq: "/projects/astrid-tech/" }) {
      ...GeneratorProject
    }
    segway: project(slug: { eq: "/projects/segway-bot/" }) {
      ...GeneratorProject
    }
    collisionZone: project(slug: { eq: "/projects/collision-zone/" }) {
      ...GeneratorProject
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

declare global {
  interface Window {
    injectResumeData?: (data: UserInjected) => void
  }
}

export default ({ data }: PageProps<Query>) => {
  const [cookie, setCookie] = useCookies(["resumeInjection"])
  console.log(data)

  var injection: UserInjected = cookie.resumeInjection ?? {}

  const resume: Resume = {
    email: "astrid@astrid.tech",
    schools: [data.calpoly],
    work: [data.microvu, data.fabtime, data.ironPanthers],
    projects: [data.astridTech, data.collisionZone, data.segway],
    ...injection,
  }

  const generators = [
    new TextResumeGenerator("md", "Markdown", generateMarkdownResume),
    new TextResumeGenerator("json", "JSON", generateJSONResume),
  ]

  useEffect(() => {
    window.injectResumeData = (data: UserInjected) => {
      setCookie("resumeInjection", JSON.stringify(data), {
        maxAge: 365 * 24 * 3600,
      })
      console.log(
        "Successfully injected the following object: ",
        data,
        "Have a nice day, Astrid!"
      )
    }
    return () => {
      window.injectResumeData = undefined
    }
  }, [])

  return (
    <Layout>
      <SEO title="Resume" description="My Resume" />
      <main>
        <Container>
          <h1>Resume Generator</h1>
          <Row>
            <Col>
              <ResumeGenerationView resume={resume} generators={generators} />
            </Col>
          </Row>
        </Container>
      </main>
    </Layout>
  )
}
