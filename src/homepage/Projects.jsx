import React, { useState } from "react";
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
  Badge,
  UncontrolledTooltip,
} from "reactstrap";
import { projects } from "../db";
import { SkillsList, getUniqueId } from "../util";

function StatusBadge({ status }) {
  const [badgeId] = useState(`badge-${getUniqueId()}`);
  var title, tooltip;
  switch (status) {
    case "wip":
      title = "WIP";
      tooltip = "I am currently working on this.";
      break;
    case "complete":
      title = "Complete";
      tooltip = "This project is complete!";
      break;
    case null:
      return "";
    default:
      throw new Error(`No status ID defined for "${status}"`);
  }
  return (
    <>
      <Badge id={badgeId} color="success">
        {title}
      </Badge>
      <UncontrolledTooltip placement="top" target={badgeId}>
        {tooltip}
      </UncontrolledTooltip>
    </>
  );
}

function ProjectCard({ project }) {
  const { img, status, title, desc, url, skills, source } = project;
  const headerSection = (
    <>
      <h5>
        {title} <StatusBadge status={status} />
      </h5>
      <CardSubtitle>{desc}</CardSubtitle>
    </>
  );
  const bodySection = (
    <>
      <SkillsList skills={skills} />
      {url ? <CardLink href={url}>{url}</CardLink> : ""}
      {source ? <CardLink href={source}>Source Code</CardLink> : ""}
    </>
  );
  return img ? (
    <Card>
      <CardBody>{headerSection}</CardBody>
      <CardImg src={img} />
      <CardBody>{bodySection}</CardBody>
    </Card>
  ) : (
    <Card>
      <CardBody>
        {headerSection}
        {bodySection}
      </CardBody>
    </Card>
  );
}

function ProjectsSection() {
  return (
    <section>
      <Container>
        <h2>Projects</h2>
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
