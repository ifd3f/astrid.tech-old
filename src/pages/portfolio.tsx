import { graphql, PageProps } from "gatsby"
import React, { useState, FC } from "react"
import { CardColumns, Container, Row, Col } from "reactstrap"
import Layout from "../components/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { Project } from "../types"
import Masonry from "react-masonry-component"
import { Timeline, IntervalNode } from "../components/timeline"
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
    <Col xs={12} sm={6} xl={4}>
      <ProjectCard project={project} hovered={false} />
    </Col>
  ))
  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container fluid>
        <Row>
          <Col lg={2}></Col>
          <Col lg={10}>
            <Masonry>{cards}</Masonry>
          </Col>
        </Row>
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
