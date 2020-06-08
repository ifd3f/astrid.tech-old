import { graphql, Link, PageProps } from "gatsby"
import React from "react"
import Layout from "../components/layout"
import SEO from "../components/seo"
import { rhythm } from "../utils/typography"
import { Project } from "../types"
import { ProjectCard } from "../components/project"
import { CardColumns, Container } from "reactstrap"

type Data = {
  site: {
    siteMetadata: {
      title: string
    }
  }
  allMarkdownRemark: {
    edges: {
      node: {
        excerpt: string
        frontmatter: Project
        fields: {
          slug: string
        }
      }
    }[]
  }
}

export const pageQuery = graphql`
  {
    site {
      siteMetadata {
        title
      }
    }
    allMarkdownRemark(
      filter: { frontmatter: { type: { eq: "project" } } }
      sort: { fields: [frontmatter___startDate], order: DESC }
    ) {
      edges {
        node {
          fields {
            slug
          }
          frontmatter {
            startDate(formatString: "YYYY-MM")
            endDate(formatString: "YYYY-MM")
            title
            description
            status
            tags
            url
            source
          }
        }
      }
    }
  }
`

const ProjectsIndex = ({ data }: PageProps<Data>) => {
  const posts = data.allMarkdownRemark.edges

  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container fluid>
        <CardColumns>
          {posts.map(({ node }) => {
            return <ProjectCard project={node.frontmatter}></ProjectCard>
          })}
        </CardColumns>
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
