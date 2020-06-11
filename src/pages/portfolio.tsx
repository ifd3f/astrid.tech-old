import { graphql, PageProps } from "gatsby"
import React from "react"
import { CardColumns, Container, Row, Col } from "reactstrap"
import Layout from "../components/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { Project } from "../types"
import Masonry from "react-masonry-component"
type Data = {
  site: {
    siteMetadata: {
      title: string
    }
  }
  allProject: {
    edges: [
      {
        node: Project
      }
    ]
  }
}

export const pageQuery = graphql`
  {
    site {
      siteMetadata {
        title
      }
    }
    allProject(sort: { fields: [endDate], order: DESC }) {
      edges {
        node {
          slug
          thumbnailPublicPath
          startDate(formatString: "YYYY-MM")
          endDate(formatString: "YYYY-MM")
          title
          description
          status
          tags {
            tag {
              name
              color
              textColor
              slug
            }
          }
          url
          source
        }
      }
    }
  }
`

const ProjectsIndex = ({ data }: PageProps<Data>) => {
  const projects = data.allProject.edges.map(edge => edge.node)

  const cards = projects.map(project => (
    <Col xs={3}>
      <ProjectCard project={project} />
    </Col>
  ))

  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container fluid>
        <Masonry>{cards}</Masonry>
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
