import React from "react"
import { ExperienceSection, HeadingSection } from "../components/homepage"
import Layout from "../components/layout"
import SEO from "../components/seo"
import { EducationSection } from "../components/homepage/education"

const Homepage = () => {
  return (
    <Layout>
      <SEO title="Astrid Yu" />
      <HeadingSection />
      <EducationSection />
      <ExperienceSection />
    </Layout>
  )
}

export default Homepage
