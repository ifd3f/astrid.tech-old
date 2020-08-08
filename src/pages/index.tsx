import React from "react"
import HomepageContainer from "../components/homepage"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import ElevatorSection from "../components/homepage/elevator"

const Homepage = () => {
  return (
    <Layout>
      <SEO
        title="Astrid Yu"
        description="My name is Astrid Yu and I create software. This is my website, and it's currently under construction!"
      />
      <HomepageContainer />
    </Layout>
  )
}

export default Homepage
