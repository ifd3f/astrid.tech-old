import React from "react";
import { Container, Row } from "reactstrap";
import { Project } from "../../types/types";
import { ALink } from "../util/boilerplate";
import { HomepageSection } from "./util";

type QueryData = {
  allProject: {
    edges: {
      node: Project;
    }[];
  };
};

export function ProjectsSection() {
  return (
    <HomepageSection style={{ backgroundColor: "#f0d0b6" }}>
      <div className="">
        <h2>Featured Projects</h2>
        <ALink href="/projects">See more</ALink>
      </div>
      <Container fluid className="projectShowcase">
        <Row>
          {/*result.allProject.edges.map(({ node: project }) => (
            <Col key={project.slug} lg={4}>
              <ProjectCard project={project} />
            </Col>
          ))*/}
        </Row>
      </Container>
    </HomepageSection>
  );
}
