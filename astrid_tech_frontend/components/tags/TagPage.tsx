/*

import { format } from "date-fns";
import Link from "next/link";
import React, { FC } from "react";
import Masonry from "react-masonry-component";
import {
  Badge,
  Card,
  CardBody,
  CardFooter,
  CardHeader,
  Col,
  Container
} from "reactstrap";
import {
  blogSlugToString,
  formatDateInterval,
  getBlogSlug
} from "../../lib/util";
import { BlogPost, Project } from "../../types/types";
import Layout from "../layout/layout";
import SEO from "../seo";
import { TagBadge, TagList } from "./tag";
import style from "./tag.module.scss";

type SiteObject<T, TypeString> = T & {
  sortDate: Date;
  __typename: TypeString;
};

type SiteObjectUnion =
  | SiteObject<Project, "Project">
  | SiteObject<BlogPost, "BlogPost">;

type Context = {
  id: string;
};

type SiteObjectDisplayProps = {
  object: SiteObjectUnion;
};

const dateClassName = `text-muted ${style.date}`;

const BlogPostDisplay: FC<{ post: BlogPost }> = ({ post }) => {
  const slug = blogSlugToString(getBlogSlug(post));
  return (
    <Card>
      <Link href={slug}>
        <a className={style.cardLink} href={slug}>
          <CardHeader>
            <h5>
              {post.title} <Badge color="success">Blog</Badge>
            </h5>
            <p className={dateClassName}>{format(post.date, "d MMM yyyy")}</p>
          </CardHeader>
          <CardBody>
            <div className="lead">{post.description}</div>
            <small className="text-muted">{post.excerpt}</small>
          </CardBody>
        </a>
      </Link>
      <CardFooter>
        <TagList tags={post.tags} limit={7} link />
      </CardFooter>
    </Card>
  );
};

const ProjectDisplay: FC<{ project: Project }> = ({ project }) => {
  return (
    <Card>
      <Link className={style.cardLink} to={project.slug}>
        <CardHeader>
          <h5>
            {project.title} <Badge color="primary">Project</Badge>
          </h5>
          <p className={dateClassName}>
            {formatDateInterval(project.startDate, project.endDate)}
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
  );
};

const SiteObjectDisplay: FC<SiteObjectDisplayProps> = ({ object }) => {
  switch (object.__typename) {
    case "BlogPost":
      return <BlogPostDisplay post={object} />;
    case "Project":
      return <ProjectDisplay project={object} />;
  }
};

export type TagPageProps = {
  objects: 
}

const TagDetailTemplate: FC<{}> = ({ data }) => {
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
            {objects.map((object) => (
              <Col xs="12" md="6" lg="4" style={{ paddingBottom: 30 }}>
                <SiteObjectDisplay object={object} />
              </Col>
            ))}
          </Masonry>
        </section>
      </Container>
    </Layout>
  );
};

export default TagDetailTemplate;
*/
