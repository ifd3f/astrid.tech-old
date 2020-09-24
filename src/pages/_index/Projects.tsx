import { graphql, Link, useStaticQuery } from "gatsby"
import React from "react"
import { Col, Container, Row } from "reactstrap"
import { ProjectCard } from "src/components/project"
import { Project } from "../../types"
import { HomepageSection } from "./util"

type QueryData = {
  allProject: {
    edges: {
      node: Project
    }[]
  }
}

export function ProjectsSection() {
  const result: QueryData = useStaticQuery(graphql`
    query GetFeaturedProjects {
      allProject(
        sort: { fields: [endDate], order: DESC }
        filter: {
          slug: {
            in: [
              "/project/inventree/"
              "/project/astrid-tech/"
              "/project/hairnet/"
              "/project/collision-zone/"
            ]
          }
        }
      ) {
        edges {
          node {
            ...ProjectCard
          }
        }
      }
    }
  `)

  return (
    <HomepageSection style={{ backgroundColor: "#f0d0b6" }}>
      <div className="">
        <h2>Featured Projects</h2>
        <Link to="/portfolio">See more</Link>
      </div>
      <Container fluid className="projectShowcase">
        <Row>
          {result.allProject.edges.map(({ node: project }) => (
            <Col key={project.slug} lg={4}>
              <ProjectCard project={project} />
            </Col>
          ))}
        </Row>
      </Container>
    </HomepageSection>
  )
}
