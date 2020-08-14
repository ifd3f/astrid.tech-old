import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import { PostBrief } from "../components/blog"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { BlogPost } from "../types/index"
import { Container } from "reactstrap"
import styles from "../scss/blog.module.scss"
import { BsEnvelope, BsPhone } from "react-icons/bs"
import { GiPhone } from "react-icons/gi"
import { GoMarkGithub } from "react-icons/go"

type Data = {
  file: { childMarkdownRemark: { html: string } }
}

export const pageQuery = graphql`
  query {
    file(relativePath: { eq: "text/about.md" }) {
      childMarkdownRemark {
        html
      }
    }
  }
`
const About: FC<PageProps<Data>> = props => {
  const { data } = props
  // TODO add linkedin when... well, you know
  return (
    <Layout {...props} currentLocation="about">
      <SEO
        title="About"
        description="Information about the not-particularly-illustrious person known as Astrid Yu."
      />
      <Container className={styles.blogContentContainer}>
        <div
          dangerouslySetInnerHTML={{
            __html: data.file.childMarkdownRemark.html,
          }}
        />
        <div>
          <ul>
            <li>
              <a href="mailto:hello@astrid.tech">
                <BsEnvelope title="Email" /> hello@astrid.tech
              </a>
            </li>
            <li>
              <a href="tel:+18052705368">
                <GiPhone title="Phone" /> ‪(805) 270-5368‬
              </a>
            </li>
            <li>
              <a href="https://github.com/Plenglin">
                <GoMarkGithub title="GitHub" /> Follow me on GitHub
              </a>
            </li>
          </ul>
        </div>
      </Container>
    </Layout>
  )
}

export default About
