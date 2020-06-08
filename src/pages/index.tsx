import React from "react"
import { ExperienceSection, HeadingSection } from "../components/homepage"
import Layout from "../components/layout"
import SEO from "../components/seo"

const Homepage = () => {
  return (
    <Layout>
      <SEO title="Astrid Yu" />
      <HeadingSection />
      <ExperienceSection />
    </Layout>
  )
}

export default Homepage
