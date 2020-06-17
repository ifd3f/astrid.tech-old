import React from "react"
import {
  ExperienceSection,
  EducationSection,
  HeadingSection,
  SkillsSection,
  ProjectsSection,
} from "../components/homepage"
import Layout from "../components/layout"
import SEO from "../components/seo"

const Homepage = () => {
  return (
    <Layout>
      <SEO title="Astrid Yu" />
      <HeadingSection />
      <SkillsSection />
      <EducationSection />
      <ExperienceSection />
      <ProjectsSection />
    </Layout>
  )
}

export default Homepage
