import { navigate } from "gatsby"
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
  status: null | "wip" | "complete" | "scrapped"
}

export const StatusBadge: FC<StatusBadgeProps> = ({ status }) => {
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
      color = "danger"
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
  hovered?: boolean
  onMouseEnter?: (project: Project) => void
  onMouseLeave?: (project: Project) => void
}

export const ProjectCard: FC<ProjectCardProps> = ({
  project,
  hovered = false,
  onMouseEnter: _onEnter,
  onMouseLeave: _onLeave,
}) => {
  const headerSection = (
    <>
      <h5>
        {project.title} <StatusBadge status={project.status} />
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
  const onClickCard = (ev: React.MouseEvent<HTMLElement>) => {
    if (ev.target.tagName != "A") {
      navigate(project.slug, { replace: false })
    }
  }

  const onEnter = () => {
    _onEnter && _onEnter(project)
  }

  const onExit = () => {
    _onLeave && _onLeave(project)
  }

  const card = project.thumbnailPublicPath ? (
    <Card className={styles.projectCard}>
      <CardBody>{headerSection}</CardBody>
      <CardImg src={project.thumbnailPublicPath} />
      <CardBody>{bodySection}</CardBody>
    </Card>
  ) : (
    <Card className={styles.projectCard}>
      <CardBody>
        {headerSection}
        {bodySection}
      </CardBody>
    </Card>
  )

  const className = hovered ? styles.hovered : ""

  return (
    <div
      className={className}
      onClick={onClickCard}
      onMouseEnter={onEnter}
      onMouseLeave={onExit}
    >
      {card}
    </div>
  )
}
