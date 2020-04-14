import React, { useState } from "react";
import {
  Badge,
  Card,
  CardBody,
  CardColumns,
  CardImg,
  CardLink,
  CardSubtitle,
  Container,
  UncontrolledTooltip,
} from "reactstrap";
import { projects } from "../db";
import { getUniqueId, SkillsList, LoadOnView } from "../util";
import style from "./style.module.scss";
import { LazyImg } from "../util/index";

function StatusBadge({ status }) {
  const [badgeId] = useState(`badge-${getUniqueId()}`);
  var title, tooltip, color;
  switch (status) {
    case "wip":
      title = "WIP";
      tooltip = "I am currently working on this.";
      color = "info";
      break;
    case "complete":
      title = "Complete";
      tooltip = "This project is complete!";
      color = "success";
      break;
    case null:
      return "";
    default:
      throw new Error(`No status ID defined for "${status}"`);
  }
  return (
    <>
      <Badge id={badgeId} color={color}>
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
      <LoadOnView>
        <CardImg src={img} />
      </LoadOnView>
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
      <div className={style.sectionHeading}>
        <h2>Projects</h2>
      </div>
      <Container fluid>
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
