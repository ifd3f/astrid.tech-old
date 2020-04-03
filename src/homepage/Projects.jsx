import React from "react";
import { Container } from "reactstrap";

function ProjectCard({ project }) {
  const { img, title, shortDesc, tags } = project;
  return <Card></Card>;
}

function ProjectsSection() {
  return (
    <section>
      <Container></Container>
    </section>
  );
}

export default ProjectsSection;
