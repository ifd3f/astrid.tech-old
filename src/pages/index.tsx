import React from "react"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { Heading } from "./_index/_layout"

const Homepage = () => {
  return (
    <Layout>
      <SEO
        title="Astrid Yu"
        description="My name is Astrid Yu and I create software. This is my website, and it's currently under construction!"
      />
      <Heading backgroundColor="#F7A8B8"></Heading>
    </Layout>
  )
}

export default Homepage
