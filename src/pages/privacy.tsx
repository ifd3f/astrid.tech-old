import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { PostBrief } from "../components/blog"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { BlogPost } from "../types/index"
import { Container } from "reactstrap"
import styles from "../scss/blog.module.scss"

type Data = {
  file: { childMarkdownRemark: { html: string } }
}

export const pageQuery = graphql`
  query {
    file(relativePath: { eq: "text/privacy.md" }) {
      childMarkdownRemark {
        html
      }
    }
  }
`

const PrivacyPolicy: FC<PageProps<Data>> = props => {
  const { data } = props
  return (
    <Layout {...props}>
      <SEO title="Blog" />
      <Container className={styles.blogContentContainer}>
        <div
          dangerouslySetInnerHTML={{
            __html: data.file.childMarkdownRemark.html,
          }}
        />
      </Container>
    </Layout>
  )
}

export default PrivacyPolicy
