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
  switch (status) {
    case "wip":
      return (
        <>
          <Badge id={badgeId} color="primary">
            WIP
          </Badge>
          <UncontrolledTooltip placement="top" target={badgeId}>
            I am actively working on this right now.
          </UncontrolledTooltip>
        </>
      );
    case "partial":
      return (
        <>
          <Badge id={badgeId} color="warning">
            Partial
          </Badge>
          <UncontrolledTooltip placement="top" target={badgeId}>
            I am not working on this, but it is partially complete. There may be
            working results you can see in action.
          </UncontrolledTooltip>
        </>
      );
    case "complete":
      return (
        <>
          <Badge id={badgeId} color="success">
            Complete
          </Badge>
          <UncontrolledTooltip placement="top" target={badgeId}>
            This project is in a viable state. I may release updates to it every
            now and then.
          </UncontrolledTooltip>
        </>
      );
    case "canceled":
      return (
        <>
          <Badge id={badgeId} color="danger">
            Canceled
          </Badge>
          <UncontrolledTooltip placement="top" target={badgeId}>
            This project was left abandoned because I decided it was no longer
            viable or possible to finish.
          </UncontrolledTooltip>
        </>
      );
    default:
      throw new Error(`No status ID defined for "${status}"`);
  }
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
