import React, { useState } from "react"
import { Container, Row, Col } from "reactstrap"
import { useStaticQuery, graphql, Link } from "gatsby"
import { ProjectCard } from "../project"
import { Project } from "../../types"
import { HomepageSection } from "./util"

type QueryData = {
  allProject: {
    edges: {
      node: Project
    }[]
  }
}

const ProjectsSection = () => {
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
    <HomepageSection color="#5e0e59">
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

export default ProjectsSection
