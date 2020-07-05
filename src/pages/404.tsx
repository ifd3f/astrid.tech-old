import React, { FC } from "react"
import { graphql, PageProps } from "gatsby"

import Layout from "../components/layout/layout"
import SEO from "../components/seo"

const NotFoundPage: FC<PageProps> = props => {
  const { data, location } = props
  const siteTitle: string = (data as any).site.siteMetadata.title

  return (
    <Layout {...props}>
      <SEO title="404: Not Found" />
      <h1>Not Found</h1>
      <p>You just hit a route that doesn&#39;t exist... the sadness.</p>
    </Layout>
  )
}

export default NotFoundPage

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        title
      }
    }
  }
`
