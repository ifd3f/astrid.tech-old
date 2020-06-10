import { Link, navigate } from "gatsby"
import React, { FC, useState } from "react"
import {
  Badge,
  Card,
  CardBody,
  CardImg,
  CardLink,
  CardSubtitle,
  UncontrolledTooltip,
} from "reactstrap"
import { Project } from "../types"
import styles from "./project.module.scss"
import { getUniqueId, LoadOnView, TagList } from "./util"

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

const ProjectCardImg = () => {}

type ProjectCardProps = {
  project: Project
}

export const ProjectCard: FC<ProjectCardProps> = ({ project }) => {
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
      <TagList tags={project.tags.map(t => t.tag!!)} />
      {project.url ? <CardLink href={project.url}>{project.url}</CardLink> : ""}
      {project.source.map(url => (
        <CardLink href={url}>Source Code</CardLink>
      ))}
    </>
  )
  const onClickCard = () => {
    navigate(project.slug)
  }

  return project.thumbnailPublicPath ? (
    <Card onClick={onClickCard} className={styles.projectCard}>
      <CardBody>{headerSection}</CardBody>
      <LoadOnView>
        <CardImg src={project.thumbnailPublicPath} />
      </LoadOnView>
      <CardBody>{bodySection}</CardBody>
    </Card>
  ) : (
    <Card onClick={onClickCard} className={styles.projectCard}>
      <CardBody>
        {headerSection}
        {bodySection}
      </CardBody>
    </Card>
  )
}
