import { Project } from "../types"
import React, { useState, FC } from "react"
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
} from "reactstrap"
import { getUniqueId, LoadOnView, TagsList } from "./util"

type StatusBadgeProps = {
  status: string
}

const StatusBadge: FC<StatusBadgeProps> = ({ status }) => {
  const [badgeId] = useState(`status-badge-${getUniqueId()}`)
  let title: string, tooltip: string, color: string
  switch (status) {
    case "wip":
      title = "WIP"
      tooltip = "I am currently working on this."
      color = "info"
      break
    case "complete":
      title = "Complete"
      tooltip = "This project is complete!"
      color = "success"
      break
    case "scrapped":
      title = "Scrapped"
      tooltip = "I decided it wasn't worth pursuing anymore."
      color = "error"
      break
    default:
      return null
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
  )
}

type ProjectCardProps = {
  project: Project
}

export const ProjectCard: FC<ProjectCardProps> = ({ project }) => {
  console.log(project)
  const headerSection = (
    <>
      <h5>
        {project.title} <StatusBadge status={status} />
      </h5>
      <CardSubtitle>{project.description}</CardSubtitle>
    </>
  )
  const bodySection = (
    <>
      {project.url ? <CardLink href={project.url}>{project.url}</CardLink> : ""}
      {project.source.map(url => (
        <CardLink href={url}>Source Code</CardLink>
      ))}
    </>
  )
  return false ? (
    <Card>
      <CardBody>{headerSection}</CardBody>
      <LoadOnView>
        <CardImg src={"img"} />
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
  )
}
