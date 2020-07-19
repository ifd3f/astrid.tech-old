import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import Masonry from "react-masonry-component"
import { Col, Container, Row } from "reactstrap"
import Layout, { MainNavbar } from "../components/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { TagBadge } from "../components/tag"
import { Project } from "../types"
import { Tag } from "../types/index"
import styles from "./projects.module.scss"

type Data = {
  site: {
    siteMetadata: {
      title: string
    }
  }
  allProject: {
    edges: {
      node: Project
    }[]
  }
}

export const pageQuery = graphql`
  {
    site {
      siteMetadata {
        title
      }
    }
    allProject(sort: { fields: [endDate], order: DESC }) {
      edges {
        node {
          ...ProjectCard
        }
      }
    }
  }
`

type TagsFilterBarProps = {
  tagSet: Tag[]
  select: (tag: Tag) => void
  deselect: (tag: Tag) => void
}

const TagsFilterBar: FC<TagsFilterBarProps> = ({
  tagSet,
  select,
  deselect,
}) => {
  return tagSet.map(tag => (
    <div>
      <TagBadge tag={tag} />
    </div>
  ))
}

type ProjectCardSwimlaneProps = {
  projects: Project[]
  title: string
}

export const ProjectCardSwimlane: FC<ProjectCardSwimlaneProps> = ({
  projects,
  title,
}) => {
  return (
    <div className={styles.swimlane}>
      <h2>{title}</h2>
      <div className={styles.swimlaneScroll}>
        <div className={styles.swimlaneContents}>
          {projects.map(project => (
            <ProjectCard project={project} />
          ))}
        </div>
      </div>
    </div>
  )
}

type ProjectCardContainerProps = {
  projects: Project[]
}

export const ProjectCardContainer: FC<ProjectCardContainerProps> = ({
  projects,
}) => {
  return (
    <div className={styles.outerContainer}>
      <Row>
        <Col>
          <ProjectCardSwimlane
            projects={projects.filter(project => project.status == "wip")}
            title="In Progress"
          />
        </Col>
        <Col>
          <ProjectCardSwimlane
            projects={projects.filter(project => project.status == "complete")}
            title="Complete"
          />
        </Col>
        <Col>
          <ProjectCardSwimlane
            projects={projects.filter(
              project => project.status != "complete" && project.status != "wip"
            )}
            title="Other"
          />
        </Col>
      </Row>
    </div>
  )
}

const ProjectsIndex: FC<PageProps<Data>> = ({ data }) => {
  const projects = data.allProject.edges.map(edge => edge.node)

  const cards = projects.map(project => (
    <Col className={styles.projectCardWrapper} xs={12} sm={6} xl={4}>
      <ProjectCard project={project} hovered={false} />
    </Col>
  ))
  return (
    <div>
      <SEO title="Projects" />
      <MainNavbar highlighted="projects" />
      <Container
        className={styles.portfolioContainer}
        fluid
        style={{ height: "100%" }}
      >
        <ProjectCardContainer projects={projects} />
      </Container>
    </div>
  )
}

export default ProjectsIndex
