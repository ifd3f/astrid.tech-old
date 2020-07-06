import { navigate, graphql } from "gatsby"
import React, { FC, useState, useEffect } from "react"
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
import { getUniqueId, LoadOnView } from "./util"
import { TagList } from "./tag"

type StatusBadgeProps = {
  status: null | "wip" | "complete" | "scrapped"
}

export const StatusBadge: FC<StatusBadgeProps> = ({ status }) => {
  const [badgeId, setBadgeId] = useState<string | null>(null)
  useEffect(() => {
    setBadgeId(`status-badge-${getUniqueId()}`)
  }, [])

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
      {badgeId ? (
        <UncontrolledTooltip placement="top" target={badgeId}>
          {tooltip}
        </UncontrolledTooltip>
      ) : null}
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

export const projectCardFragment = graphql`
  fragment ProjectCard on Project {
    slug
    thumbnailPublicPath
    startDate(formatString: "YYYY-MM")
    endDate(formatString: "YYYY-MM")
    title
    description
    status
    tags {
      tag {
        ...TagBadge
      }
    }
    url
    source
  }
`
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
      {project.source.length > 0 ? (
        <CardLink href={project.source[0]}>Source Code</CardLink>
      ) : null}
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

  const className =
    (hovered ? styles.hoveredProjectCard : "") + " " + styles.projectCard

  const card = project.thumbnailPublicPath ? (
    <Card className={className}>
      <CardBody>{headerSection}</CardBody>
      <CardImg src={project.thumbnailPublicPath} />
      <CardBody>{bodySection}</CardBody>
    </Card>
  ) : (
    <Card className={className}>
      <CardBody>
        {headerSection}
        {bodySection}
      </CardBody>
    </Card>
  )

  return (
    <div onClick={onClickCard} onMouseEnter={onEnter} onMouseLeave={onExit}>
      {card}
    </div>
  )
}
