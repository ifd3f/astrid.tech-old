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
import ElevatorSection from "../components/homepage/elevator"

const Homepage = () => {
  return (
    <Layout>
      <SEO title="Astrid Yu" />
      <HeadingSection />
      <ElevatorSection />
      <SkillsSection />
      <EducationSection />
      <ExperienceSection />
      <ProjectsSection />
    </Layout>
  )
}

export default Homepage
