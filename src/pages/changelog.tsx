import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { PostBrief } from "../components/blog"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { BlogPost } from "../types/index"
import { Container } from "reactstrap"
import styles from "../scss/blog.module.scss"

type Data = {
  allGitCommit: {
    edges: {
      node: {
        refs: string
        date: string
      }
    }[]
  }
  changelogYaml: {
    major: number
    minors: {
      minor: number
      patches: {
        patch: number
        bugfixes: string[]
        features: string[]
        internals: string[]
      }
    }[]
  }
}

export const pageQuery = graphql`
  {
    allGitCommit(filter: { refs: { glob: "*tag: v*.*.*" } }) {
      edges {
        node {
          refs
          date(formatString: "YYYY MMMM DD")
        }
      }
    }
    changelogYaml {
      major
      minors {
        minor
        patches {
          bugfixes
          features
          internals
          patch
        }
      }
    }
  }
`

type Entry = {
  version: string
  date: string
  features: string[]
  internal: string[]
  bugfixes: string[]
}

const PrivacyPolicy: FC<PageProps<Data>> = props => {
  const { data } = props

  const versionToCommit = new Map<string, string>()

  return (
    <Layout {...props}>
      <SEO title="Changelog" />
      <Container className={styles.blogContentContainer}></Container>
    </Layout>
  )
}

export default PrivacyPolicy
