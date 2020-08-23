import { graphql, navigate } from "gatsby"
import GatsbyImage from "gatsby-image"
import React, { FC, PropsWithChildren, useEffect, useState } from "react"
import { Badge, CardBody, CardLink, UncontrolledTooltip } from "reactstrap"
import { Project } from "../../types"
import { TagList } from "../tag"
import { getPersistentColor, getUniqueId, PastelTheme } from "src/util"
import styles from "./project.module.scss"
import BackgroundImage from "gatsby-background-image"

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
    <Badge id={badgeId} color={color}>
      {title}
      {badgeId ? (
        <UncontrolledTooltip placement="top" target={badgeId}>
          {tooltip}
        </UncontrolledTooltip>
      ) : null}
    </Badge>
  )
}

const ProjectCardImg = () => {}

type PropsWithProject = {
  project: Project
}

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
    thumbnail {
      childImageSharp {
        fluid(maxWidth: 400, maxHeight: 160) {
          ...GatsbyImageSharpFluid
        }
      }
      publicURL
    }
  }
`

type ProjectTitleProps = {
  project: Project
  titleBorder?: boolean
}

const ProjectTitle: FC<ProjectTitleProps> = ({ project, titleBorder }) => (
  <h3 className={`${styles.title} ${titleBorder ? styles.border : ""}`}>
    {project.title}
  </h3>
)

type ProjectBodyProps = {
  project: Project
  showBorder?: boolean
}

const ProjectBody: FC<ProjectBodyProps> = ({ project, showBorder = false }) => (
  <TagList
    tags={project.tags}
    limit={5}
    className={styles.tags + (showBorder ? " " + styles.border : "")}
    link
  />
)

export const ProjectCard: FC<ProjectCardProps> = ({
  project,
  hovered = false,
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

  const [h, s, v] = getPersistentColor(project.slug, PastelTheme)
  if (project.thumbnail && project.thumbnail.childImageSharp) {
    const stack = [
      `linear-gradient(to bottom, hsla(${h}, 80%, 100%, 0.9), hsla(${h}, 80%, 80%, 0.7), rgba(0.6, 0.6, 0.6, 0.3))`,
      (project.thumbnail as any).childImageSharp.fluid,
    ]
    return (
      <BackgroundImage
        fluid={stack}
        {...cardOuterProps}
        style={{
          backgroundColor: "clear",
        }}
      >
        <CardBody>
          <ProjectTitle project={project} />
          <ProjectBody project={project} />
        </CardBody>
      </BackgroundImage>
    )
  } else {
    const color = `hsl(${h}, ${s}%, ${v}%)`
    return (
      <div
        {...cardOuterProps}
        style={{ backgroundColor: color, borderColor: color }}
      >
        <CardBody>
          <ProjectTitle project={project} />
          <ProjectBody project={project} />
        </CardBody>
      </div>
    )
  }
}
