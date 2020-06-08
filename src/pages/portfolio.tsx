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
          thumbnailPublicPath: string | null
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
            thumbnailPublicPath
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
  const projects = data.allMarkdownRemark.edges.map(edge => {
    const frontmatter = edge.node.frontmatter
    if (edge.node.fields.thumbnailPublicPath) {
      frontmatter.thumbnailURL = edge.node.fields.thumbnailPublicPath
    }
    return frontmatter
  })

  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container fluid>
        <CardColumns>
          {projects.map(project => (
            <ProjectCard project={project} />
          ))}
        </CardColumns>
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
