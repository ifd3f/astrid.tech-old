import React from "react";
import {
  Container,
  CardHeader,
  CardBody,
  Card,
  CardSubtitle,
  CardImg,
  CardLink,
  CardDeck,
  CardColumns,
} from "reactstrap";
import { projects } from "../db";
import { SkillsList } from "../util";

function ProjectCard({ project }) {
  const { img, title, desc, url, skills, source } = project;
  return (
    <Card>
      <CardHeader>
        <h5>{title}</h5>
        <CardSubtitle>{desc}</CardSubtitle>
      </CardHeader>
      {img ? <CardImg src={img} /> : ""}
      <CardBody>
        <SkillsList skills={skills} />
        {url ? <CardLink href={url}>{url}</CardLink> : ""}
        {source ? <CardLink href={source}>Source Code</CardLink> : ""}
      </CardBody>
    </Card>
  );
}

function ProjectsSection() {
  return (
    <section>
      <Container>
        <h2>In my free time, I like creating projects to learn new things</h2>
        <CardColumns>
          {Array.from(projects.values()).map((p) => (
            <ProjectCard key={p.id} project={p} />
          ))}
        </CardColumns>
      </Container>
    </section>
  );
}

export default ProjectsSection;
