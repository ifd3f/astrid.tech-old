import { graphql, PageProps, useStaticQuery } from "gatsby"
import React, {
  FC,
  useState,
  createContext,
  PropsWithChildren,
  ReactNode,
  useContext,
  ChangeEvent,
  ChangeEventHandler,
} from "react"
import { Col, Container, Row, Jumbotron, InputGroup, Input } from "reactstrap"
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

type SearchContext = {
  tags: Map<string, Tag>
  projects: Map<string, Project>

  displayedProjects: Project[]

  searchString: string
  setSearchString: (searchString: string) => void

  filterTags: Tag[]
  addFilterTag: (slug: string) => void
  removeFilterTag: (slug: string) => void
  clearFilterTags: () => void

  shouldFilterAny: boolean
  setShouldFilterAny: (shouldFilterAny: boolean) => void
}

const SearchContext = createContext<SearchContext>({} as any)

type FiltererArgs = {
  children: ReactNode
  projects: Map<string, Project>
  tags: Map<string, Tag>
  index: Index
}

const Filterer: FC<FiltererArgs> = ({ children, projects, index, tags }) => {
  const [searchString, _setSearchString] = useState("")
  const [filterTags, setFilterTags] = useState<string[]>([])
  const [shouldFilterAny, _setShouldFilterAnyTags] = useState<boolean>(false)

  const setSearchString = (searchString: string) => {
    _setSearchString(searchString)
  }

  const setShouldFilterAny = (shouldFilterAny: boolean) => {
    _setShouldFilterAnyTags(shouldFilterAny)
  }

  const addFilterTag = (slug: string) => {
    setFilterTags([...filterTags, slug])
  }

  const removeFilterTag = (slug: string) => {
    setFilterTags(filterTags.filter(tag => tag != slug))
  }

  const clearFilterTags = () => {
    setFilterTags([])
  }

  const filterTagsSet = new Set(filterTags)

  var displayedProjects = index
    .search(searchString)
    .map(result => projects.get(result.ref)!!)

  if (filterTags.length > 0) {
    displayedProjects = displayedProjects.filter(project => {
      const filteredCount = project.tags.filter(tag =>
        filterTagsSet.has(tag.slug)
      ).length

      return shouldFilterAny
        ? filteredCount > 0
        : filteredCount == filterTags.length
    })
  }

  return (
    <SearchContext.Provider
      value={{
        tags,
        projects,

        displayedProjects,

        searchString,
        setSearchString,

        filterTags: filterTags.map(slug => tags.get(slug)!!),
        addFilterTag,
        removeFilterTag,
        clearFilterTags,

        shouldFilterAny,
        setShouldFilterAny,
      }}
    >
      {children}
    </SearchContext.Provider>
  )
}
const TagsFilterBar: FC = () => {
  const { tags, setSearchString } = useContext(SearchContext)
  const onChange: ChangeEventHandler<HTMLInputElement> = ev => {
    setSearchString(ev.target.value)
  }
  return (
    <div className={styles.tagsFilterBar}>
      <Container>
        <InputGroup>
          <Input placeholder="Filter..." onChange={onChange} />
        </InputGroup>
        {[...tags.values()].map(tag => (
          <TagBadge tag={tag} />
        ))}
      </Container>
    </div>
  )
}

export const ProjectCardContainer: FC = () => {
  const { displayedProjects } = useContext(SearchContext)
  return (
    <div className={styles.cardsContainer}>
      {displayedProjects.map(project => (
        <div className={styles.projectCardWrapper}>
          <ProjectCard project={project} />
        </div>
      ))}
    </div>
  )
}

const ProjectsIndex: FC<PageProps<Data>> = ({ data }) => {
  const index = Index.load(JSON.parse(data.projectSearchIndex.data))
  const projects = new Map(
    data.allProject.edges.map(({ node }) => [node.slug, node])
  )
  const tagMap = new Map(
    Array.from(projects.values())
      .flatMap(project => project.tags)
      .map(tag => [tag.slug, tag])
  )

  const cards = Array.from(projects.values()).map(project => (
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
      <Filterer projects={projects} tags={tagMap} index={index}>
        <TagsFilterBar />
        <Container>
          <ProjectCardContainer />
        </Container>
      </Filterer>
    </Layout>
  )
}

export default ProjectsIndex
