import { graphql, Link, PageProps } from "gatsby"
import moment from "moment"
import React, { FC } from "react"
import Masonry from "react-masonry-component"
import {
  Badge,
  Card,
  CardBody,
  CardFooter,
  CardHeader,
  Col,
  Container,
} from "reactstrap"
import {
  BipartiteNode,
  orderByResistorSimilarity,
} from "src/components/tag-similarity/algorithm"
import { BlogPost, Project, Tag, Tagged } from "src/types"
import { formatDateInterval } from "src/util"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import { TagBadge, TagList } from "../components/tag"
import style from "./tag.module.scss"

export const pageQuery = graphql`
  query GetTagInfo($slug: String!) {
    allTag(
      limit: 1
      sort: { order: DESC, fields: priority }
      filter: { slug: { eq: $slug } }
    ) {
      nodes {
        ...TagBadge
        tagged {
          __typename
          ... on Project {
            title
            slug
            internal {
              description
            }
            startDate
            endDate
            markdown {
              excerpt(pruneLength: 70)
            }
            tags {
              ...TagBadge
            }
          }
          ... on BlogPost {
            title
            slug
            date
            description {
              childMarkdownRemark {
                html
              }
            }
            source {
              excerpt(pruneLength: 70)
            }
            tags {
              ...TagBadge
            }
          }
        }
      }
    }
  }
`

type Data = {
  allTag: {
    nodes: [
      Tag & {
        projects: Project[]
        blogPosts: BlogPost[]
      }
    ]
  }
}

type SiteObject<T, TypeString> = T & {
  sortDate: Date
  __typename: TypeString
}

type SiteObjectUnion =
  | SiteObject<Project, "Project">
  | SiteObject<BlogPost, "BlogPost">

type Context = {
  id: string
}

type SiteObjectDisplayProps = {
  object: SiteObjectUnion
}

function convertDateStringInterval(start: string, end?: string | null) {
  if (end) {
    return new Date(end)
  }
  const adjusted = (new Date(start).getTime() + new Date().getTime() * 3) / 4
  return new Date(adjusted)
}

const dateClassName = `text-muted ${style.date}`

const BlogPostDisplay: FC<{ post: BlogPost }> = ({ post }) => {
  return (
    <Card>
      <Link className={style.cardLink} to={post.slug}>
        <CardHeader>
          <h5>
            {post.title} <Badge color="success">Blog</Badge>
          </h5>
          <p className={dateClassName}>
            {moment(post.date).format("DD MMM YYYY")}
          </p>
        </CardHeader>
        <CardBody>
          <div
            className="lead"
            dangerouslySetInnerHTML={{
              __html: post.description.childMarkdownRemark.html,
            }}
          />
          <small className="text-muted">{post.source.excerpt}</small>
        </CardBody>
      </Link>
      <CardFooter>
        <TagList tags={post.tags} limit={7} link />
      </CardFooter>
    </Card>
  )
}

const ProjectDisplay: FC<{ project: Project }> = ({ project }) => {
  function format(date?: string | null) {
    if (date) {
      return moment(date).format("MMM YYYY")
    }
    return null
  }
  return (
    <Card>
      <Link className={style.cardLink} to={project.slug}>
        <CardHeader>
          <h5>
            {project.title} <Badge color="primary">Project</Badge>
          </h5>
          <p className={dateClassName}>
            {formatDateInterval(
              format(project.startDate)!,
              format(project.endDate)
            )}
          </p>
        </CardHeader>
        <CardBody>
          <p className="lead">{project.internal.description} </p>
          <small className="text-muted">{project.markdown.excerpt}</small>
        </CardBody>
      </Link>
      <CardFooter>
        <TagList tags={project.tags} limit={7} link />
      </CardFooter>
    </Card>
  )
}
const SiteObjectDisplay: FC<SiteObjectDisplayProps> = ({ object }) => {
  switch (object.__typename) {
    case "BlogPost":
      return <BlogPostDisplay post={object} />
    case "Project":
      return <ProjectDisplay project={object} />
  }
}

const TagDetailTemplate: FC<PageProps<Data, Context>> = ({ data }) => {
  const [tag] = data.allTag.nodes
  const projects = (tag.tagged.filter(
    t => t.__typename == "Project"
  ) as Project[]).map(
    project =>
      ({
        ...project,
        sortDate: convertDateStringInterval(project.startDate, project.endDate),
      } as SiteObjectUnion)
  )

  const blogPosts = (tag.tagged.filter(
    t => t.__typename == "BlogPost"
  ) as BlogPost[]).map(
    post =>
      ({
        ...post,
        sortDate: new Date(post.date),
      } as SiteObjectUnion)
  )

  const objects = projects
    .concat(blogPosts)
    .sort((a, b) => b.sortDate.getTime() - a.sortDate.getTime())

  const neighbors: BipartiteNode<Tagged, Tag>[] = tag.tagged
    .filter(obj => obj.tags)
    .map(obj => ({
      id: obj.slug,
      value: obj,
      neighbors: obj.tags.map(tag => ({
        id: tag.slug,
        neighbors: [],
        value: tag,
      })),
    }))
  const relatedTags = orderByResistorSimilarity(neighbors, "equal")
    .map(({ value }) => value)
    .filter(({ slug }) => slug != tag.slug)

  return (
    <Layout>
      <SEO title={tag.name!} description={`Items related to ${tag.name}`} />
      <Container tag="article">
        <header style={{ marginTop: 20 }}>
          <h1>
            Items related to <TagBadge tag={tag} />
          </h1>
        </header>
        <section>
          <h4>Similar Tags</h4>
          <p>
            <TagList tags={relatedTags} link />
          </p>
        </section>

        <section style={{ paddingBottom: 30 }}>
          <Masonry
            options={{
              transitionDuration: 0,
            }}
          >
            {objects.map(object => (
              <Col xs="12" md="6" lg="4" style={{ paddingBottom: 30 }}>
                <SiteObjectDisplay object={object} />
              </Col>
            ))}
          </Masonry>
        </section>
      </Container>
    </Layout>
  )
}

export default TagDetailTemplate
