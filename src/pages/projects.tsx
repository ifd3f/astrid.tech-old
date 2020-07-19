import { graphql, PageProps } from "gatsby"
import React, { FC } from "react"
import Masonry from "react-masonry-component"
import { Col, Container, Row } from "reactstrap"
import Layout from "../components/layout/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { TagBadge } from "../components/tag"
import { Project } from "../types"
import { Tag } from "../types/index"
import styles from "./projects.module.scss"
import { ProjectCardContainer } from "../components/project"

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

const ProjectsIndex: FC<PageProps<Data>> = ({ data }) => {
  const projects = data.allProject.edges.map(edge => edge.node)

  const cards = projects.map(project => (
    <Col className={styles.projectCardWrapper} xs={12} sm={6} xl={4}>
      <ProjectCard project={project} hovered={false} />
    </Col>
  ))
  return (
    <Layout>
      <SEO title="Portfolio" />
      <Container className={styles.portfolioContainer} fluid>
        <ProjectCardContainer projects={projects} />
      </Container>
    </Layout>
  )
}

export default ProjectsIndex
