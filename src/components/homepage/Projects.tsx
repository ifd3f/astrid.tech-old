import React, { useState } from "react"
import { Container, Row } from "reactstrap"
import { useStaticQuery, graphql } from "gatsby"

const ProjectsSection = () => {
  const result = useStaticQuery(graphql`
    {
      allProject(sort: { fields: [endDate], order: DESC }) {
        edges {
          node {
            ...ProjectCard
          }
        }
      }
    }
  `)

  return (
    <section>
      <div className="">
        <h2>Featured Projects</h2>
      </div>
      <Container fluid className="projectShowcase">
        <Row>
          {Array.from(projects.values()).map(p => (
            <ProjectCard key={p.id} project={p} />
          ))}
        </Row>
      </Container>
    </section>
  )
}

export default ProjectsSection
