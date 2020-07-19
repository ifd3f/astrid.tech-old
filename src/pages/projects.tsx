import { graphql, PageProps, useStaticQuery } from "gatsby"
import React, { FC, useState } from "react"
import { Col, Container, Row, Jumbotron } from "reactstrap"
import Layout, { MainNavbar } from "../components/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { TagBadge } from "../components/tag"
import { Project } from "../types"
import { Tag } from "../types/index"
import styles from "./projects.module.scss"
import { Index } from "lunr"

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
  projectSearchIndex: { data: string }
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
    projectSearchIndex {
      data
    }
  }
`

type TagsFilterBarProps = {
  tagSet: Set<Tag>
  select: (tag: Tag) => void
  deselect: (tag: Tag) => void
  searchIndexData: any
}

const TagsFilterBar: FC<TagsFilterBarProps> = ({
  tagSet,
  select,
  deselect,
  searchIndexData,
}) => {
  return [...tagSet].map(tag => (
    <div className={styles.tagsFilterBar}>
      <Container>
        <TagBadge tag={tag} />
      </Container>
    </div>
  ))
}

type ProjectCardContainerProps = {
  projects: Project[]
}

export const ProjectCardContainer: FC<ProjectCardContainerProps> = ({
  projects,
}) => {
  return (
    <div className={styles.cardsContainer}>
      {projects.map(project => (
        <div className={styles.projectCardWrapper}>
          <ProjectCard project={project} />
        </div>
      ))}
    </div>
  )
}

const ProjectsIndex: FC<PageProps<Data>> = ({ data }) => {
  const [index] = useState(Index.load(JSON.parse(data.projectSearchIndex.data)))
  const projects = data.allProject.edges.map(edge => edge.node)

  const tagSet = new Set(
    new Map(
      projects.flatMap(project => project.tags).map(tag => [tag.slug, tag])
    ).values()
  )

  const cards = projects.map(project => (
    <Col className={styles.projectCardWrapper} xs={12} sm={6} xl={4}>
      <ProjectCard project={project} hovered={false} />
    </Col>
  ))
  return (
    <Layout currentLocation="projects" className={`${styles.main}`}>
      <SEO title="Projects" />
      <header className={styles.header}>
        <h1>Projects</h1>
        <p>
          Below is an incomplete list of the projects I have worked on, of all
          sizes and types.
        </p>
      </header>
      <TagsFilterBar tagSet={tagSet} />
      <Container>
        <ProjectCardContainer projects={projects} />
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
