import { navigate, graphql } from "gatsby"
import React, {
  FC,
  useState,
  useEffect,
  PropsWithChildren,
  ReactNode,
} from "react"
import {
  Badge,
  Card,
  CardBody,
  CardImg,
  CardLink,
  CardSubtitle,
  UncontrolledTooltip,
} from "reactstrap"
import { Project } from "../../types"
import styles from "./project.module.scss"
import {
  getUniqueId,
  LoadOnView,
  getPersistentColor,
  PastelTheme,
} from "../util"
import { TagList } from "../tag"

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
    startDate(formatString: "MMMM YYYY")
    endDate(formatString: "MMMM YYYY")
    title
    status
    internal {
      description
    }
    tags {
      ...TagBadge
    }
    url
    source
  }
`

const ProjectCardOuter: FC<PropsWithChildren<ProjectCardProps>> = ({
  project,
  children,
  hovered,
  onMouseEnter: _onEnter,
  onMouseLeave: _onLeave,
}) => {
  const className =
    (hovered ? styles.hoveredProjectCard : "") + " " + styles.projectCard
  const onEnter = () => {
    _onEnter && _onEnter(project)
  }

  const onExit = () => {
    _onLeave && _onLeave(project)
  }

  const onClickCard = (ev: React.MouseEvent<HTMLElement>) => {
    if ((ev.target.tagName as string) != "A") {
      navigate(project.slug, { replace: false })
    }
  }

  const cardOuterProps = {
    onClick: onClickCard,
    onMouseEnter: onEnter,
    onMouseLeave: onExit,
    className: className,
  }

  if (project.thumbnail) {
    return (
      <div {...cardOuterProps}>
        <div></div>
        {children}
      </div>
    )
  } else {
    const background = getPersistentColor(project.slug, PastelTheme)
    return (
      <div {...cardOuterProps} style={{ backgroundColor: background }}>
        {children}
      </div>
    )
  }
}

export const ProjectCard: FC<ProjectCardProps> = ({
  project,
  hovered = false,
  onMouseEnter,
  onMouseLeave,
}) => {
  const headerSection = (
    <>
      <h5>
        {project.title} <StatusBadge status={project.status} />
      </h5>
      <CardSubtitle>{project.internal.description}</CardSubtitle>
    </>
  )
  const bodySection = (
    <>
      <TagList tags={project.tags.slice(0, 5)} />
      {project.url ? <CardLink href={project.url}>{project.url}</CardLink> : ""}
    </>
  )

  return (
    <ProjectCardOuter
      project={project}
      hovered={hovered}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
    >
      <CardBody>
        {headerSection}
        {bodySection}
      </CardBody>
    </ProjectCardOuter>
  )
}

type ProjectCardContainerProps = {
  projects: Project[]
}

export const ProjectCardContainer: FC<ProjectCardContainerProps> = ({
  projects,
}) => {
  return (
    <div className={styles.projectCardContainer}>
      {projects.map(project => (
        <ProjectCard project={project} />
      ))}
    </div>
  )
}
