import { graphql, PageProps } from "gatsby"
import React, { FC, useEffect, useState } from "react"
import { Container } from "reactstrap"
import { Site } from "src/types"
import StringSimilarity from "string-similarity"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"

export const pageQuery = graphql`
  query {
    allSitePage(sort: { fields: path }) {
      edges {
        node {
          path
        }
      }
    }
    site {
      siteMetadata {
        package {
          bugs {
            url
          }
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
  site: Site
}

type Result = {
  target: string
  rating: number
}

type MatchResults = {
  bestMatch: Result
  ratings: Result[]
}

const NotFoundPageContents: FC<{ suggestions: string[]; bugsURL: string }> = ({
  suggestions,
  bugsURL,
}) => (
  <div>
    <p style={{ fontSize: 20 }}>
      Perhaps you meant to go to{" "}
      <strong>
        <a href={suggestions[0]}>{suggestions[0]}</a>
      </strong>
      ? Or, maybe one of...
    </p>
    <ul>
      {suggestions.slice(1, 8).map(path => (
        <li key={path}>
          <a href={path}>{path}</a>
        </li>
      ))}
    </ul>
    <p>
      Maybe I brought you here on accident! In that event, please{" "}
      <a href={bugsURL}>file a bug report on my GitHub issues page</a> and I
      will fix it as soon as possible!
    </p>
  </div>
)

const NotFoundPage: FC<PageProps<Data>> = props => {
  const { data, location } = props

  const paths = data.allSitePage.edges.map(({ node }) => node.path)

  const [suggestions, setSuggestions] = useState(null as string[] | null)

  useEffect(() => {
    const results = StringSimilarity.findBestMatch(
      location.pathname,
      paths
    ) as MatchResults
    setSuggestions(
      results.ratings
        .sort((a, b) => b.rating - a.rating)
        .map(result => result.target)
        .slice(1, 8)
    )
  }, [suggestions])

  return (
    <Layout {...props}>
      <Container
        style={{
          padding: 20,
          height: "70vh",
        }}
        tag="main"
      >
        <h1>404 Not Found</h1>
        <SEO
          title="404 Not Found"
          description="This page doesn't exist! Panic!"
        />
        {suggestions ? (
          <NotFoundPageContents
            suggestions={suggestions}
            bugsURL={data.site.siteMetadata.package.bugs.url}
          />
        ) : null}
      </Container>
    </Layout>
  )
}

export default NotFoundPage
