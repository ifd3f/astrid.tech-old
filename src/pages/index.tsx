import React from "react"
import {
  ExperienceSection,
  EducationSection,
  HeadingSection,
  ProjectsSection,
} from "../components/homepage"
import Layout from "../components/layout"
import SEO from "../components/seo"

const Homepage = () => {
  return (
    <Layout>
      <SEO title="Astrid Yu" />
      <HeadingSection />
      <EducationSection />
      <ExperienceSection />
      <ProjectsSection />
    </Layout>
  )
}

export default Homepage
