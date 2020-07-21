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
import {
  Col,
  Container,
  Row,
  Jumbotron,
  InputGroup,
  Input,
  Badge,
} from "reactstrap"
import Layout, { MainNavbar } from "../components/layout"
import { ProjectCard } from "../components/project"
import SEO from "../components/seo"
import { TagBadge } from "../components/tag"
import { Project } from "../types"
import { Tag } from "../types/index"
import styles from "./projects.module.scss"
import Fuse from "fuse.js"
import { BsX } from "react-icons/bs"

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
  projectSearchIndex: {
    data: string
    keys: string[]
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
    projectSearchIndex {
      data
      keys
    }
  }
`

type SearchContext = {
  slugToTag: Map<string, Tag>
  tagUsageCounts: Map<string, number>
  selectableTags: Tag[]
  projects: Project[]

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
  projects: Project[]
  fuse: Fuse<Project, any>
}

const Filterer: FC<FiltererArgs> = ({ children, projects, fuse }) => {
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
  var displayedProjects =
    searchString == ""
      ? projects
      : fuse.search(searchString).map(result => {
          return result.item
        })

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

  const tagUsageCounts = countTagUsages(displayedProjects)
  const orderedTags = displayedProjects
    .flatMap(project => project.tags)
    .sort((a, b) => tagUsageCounts.get(b.slug)! - tagUsageCounts.get(a.slug)!)
  const slugToTag = new Map(orderedTags.map(tag => [tag.slug, tag]))

  return (
    <SearchContext.Provider
      value={{
        slugToTag,
        selectableTags: orderedTags,
        tagUsageCounts,
        projects,

        displayedProjects,

        searchString,
        setSearchString,

        filterTags: filterTags.map(slug => slugToTag.get(slug)!!),
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

const SelectableTagList: FC = () => {
  const { slugToTag, tagUsageCounts, addFilterTag } = useContext(SearchContext)

  return (
    <div className={styles.selectableTagsContainer}>
      {[...slugToTag.values()].map(tag => (
        <span
          className={styles.selectableTag}
          onClick={() => addFilterTag(tag.slug)}
          key={tag.slug}
        >
          <TagBadge tag={tag}>
            {" "}
            <Badge
              style={{
                color: tag.backgroundColor,
                backgroundColor: tag.color,
              }}
            >
              {tagUsageCounts.get(tag.slug)}
            </Badge>
          </TagBadge>
        </span>
      ))}
    </div>
  )
}

const CurrentlyUsedTagList: FC = () => {
  const { filterTags, removeFilterTag } = useContext(SearchContext)
  return (
    <div>
      {[...filterTags.values()].map(tag => (
        <span
          className={styles.deletableTag}
          onClick={() => removeFilterTag(tag.slug)}
          key={tag.slug}
        >
          <TagBadge tag={tag}>
            {" "}
            <BsX />
          </TagBadge>
        </span>
      ))}
    </div>
  )
}

const TagsFilterBar: FC = () => {
  const { slugToTag: tags, setSearchString } = useContext(SearchContext)
  const onChange: ChangeEventHandler<HTMLInputElement> = ev => {
    setSearchString(ev.target.value)
  }
  return (
    <section className={styles.searchSection}>
      <Container>
        <Row>
          <Col>
            <h2>Search</h2>
            <InputGroup>
              <Input
                placeholder="astrid.tech, CPE 233, React, etc."
                onChange={onChange}
              />
            </InputGroup>
            <CurrentlyUsedTagList />
          </Col>
          <Col>
            <h3>Or select tags to filter by</h3>
            <SelectableTagList />
          </Col>
        </Row>
      </Container>
    </section>
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

function countTagUsages(projects: Project[]) {
  const count = new Map<string, number>()
  for (const project of projects) {
    for (const tag of project.tags) {
      count.set(tag.slug, 1 + (count.get(tag.slug) ?? 0))
    }
  }
  return count
}

const ProjectsIndex: FC<PageProps<Data>> = ({ data }) => {
  const projects = data.allProject.edges.map(({ node }) => node)

  const index = Fuse.parseIndex(JSON.parse(data.projectSearchIndex.data))
  const fuse = new Fuse<Project, any>(
    projects,
    {
      threshold: 0.4,
      keys: ["title", "slug", "internal.description", "tags.name", "tags.slug"],
    },
    index
  )

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
      <Filterer projects={projects} fuse={fuse}>
        <TagsFilterBar />
        <Container>
          <ProjectCardContainer />
        </Container>
      </Filterer>
    </Layout>
  )
}

export default ProjectsIndex
