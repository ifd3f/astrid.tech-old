import React, { FC } from "react"
import { graphql, PageProps } from "gatsby"
import StringSimilarity from "string-similarity"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import Fuse from "fuse.js"
import { Container } from "reactstrap"

export const pageQuery = graphql`
  query {
    allSitePage(sort: { fields: path }) {
      edges {
        node {
          path
        }
      }
    }
  }
`

type SitePage = {
  path: string
}

type Data = {
  allSitePage: {
    edges: {
      node: SitePage
    }[]
  }
}

type Result = {
  target: string
  rating: number
}

type MatchResults = {
  bestMatch: Result
  ratings: Result[]
}

const NotFoundPage: FC<PageProps<Data>> = props => {
  const { data, location } = props

  const paths = data.allSitePage.edges.map(({ node }) => node.path)

  const result = StringSimilarity.findBestMatch(
    location.pathname,
    paths
  ) as MatchResults

  const suggestions = result.ratings
    .sort((a, b) => b.rating - a.rating)
    .map(result => result.target)
    .slice(1, 7)

  return (
    <Layout {...props}>
      <SEO title="404 Not Found" />
      <Container style={{ padding: 20 }}>
        <h1>404 Not Found</h1>
        <p style={{ fontSize: 20 }}>
          Perhaps you meant to go to{" "}
          <b>
            <a href={result.bestMatch.target}>{result.bestMatch.target}</a>
          </b>
          ? Or, maybe one of...
        </p>
        <ul>
          {suggestions.map(path => (
            <li key={path}>
              <a href={path}>{path}</a>
            </li>
          ))}
        </ul>
      </Container>
    </Layout>
  )
}

export default NotFoundPage
