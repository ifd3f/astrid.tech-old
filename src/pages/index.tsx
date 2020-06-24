import React from "react"
import HomepageContainer from "../components/homepage"
import Layout from "../components/layout"
import SEO from "../components/seo"
import ElevatorSection from "../components/homepage/elevator"

const Homepage = () => {
  return (
    <Layout>
      <SEO title="Astrid Yu" />
      <HomepageContainer />
    </Layout>
  )
}

export default Homepage
