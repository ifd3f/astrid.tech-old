import React from "react"
import Layout from "../components/layout"
import MainNavbar from "../components/navbar"
import { Container, Button } from "reactstrap"
import { HeadingSection, ExperienceSection } from "../components/homepage"

const Homepage = () => {
  return (
    <Layout title="Astrid Yu">
      <HeadingSection />
      <ExperienceSection />
    </Layout>
  )
}

export default Homepage
