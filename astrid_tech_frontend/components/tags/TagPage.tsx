import { format } from "date-fns";
import Link from "next/link";
import { FC } from "react";
import Masonry from "react-masonry-component";
import {
  Badge,
  Card,
  CardBody,
  CardFooter,
  CardHeader,
  Col,
  Container,
} from "reactstrap";
import { blogSlugToString, getBlogSlug } from "../../lib/util";
import {
  BlogPostMeta,
  convertBlogPostToObjectDate,
  convertProjectToObjectDate,
  ProjectMeta,
  SiteObject,
} from "../../types/types";
import { DateInterval, SemanticDate } from "../util/date-displays";
import Layout from "../layout/layout";
import SEO from "../seo";
import { TagBadge, TagList } from "./tag";
import style from "./tag.module.scss";
import { useTagTable } from "./TagTableProvider";

const dateClassName = `text-muted ${style.date}`;

type BlogPostDisplayProps = { post: BlogPostMeta<string> };

const BlogPostDisplay: FC<BlogPostDisplayProps> = ({ post: _post }) => {
  const post = convertBlogPostToObjectDate(_post);
  const slug = blogSlugToString(getBlogSlug(post));
  return (
    <Card className="h-entry">
      <Link href={slug} passHref>
        <a className={style.cardLink}>
          <CardHeader>
            <h5>
              <span className="p-name">{post.title}</span>{" "}
              <Badge color="success">Blog</Badge>
            </h5>
            <p className={dateClassName}>
              <SemanticDate date={post.date} formatStyle="d MMM yyyy" />
            </p>
          </CardHeader>
          <CardBody>
            <div className="lead">{post.description}</div>
            <small className="text-muted">{post.excerpt!!}</small>
          </CardBody>
        </a>
      </Link>
      <CardFooter>
        <TagList tags={post.tags} limit={7} link />
      </CardFooter>
    </Card>
  );
};

type ProjectDisplayProps = { project: ProjectMeta<string> };

const ProjectDisplay: FC<ProjectDisplayProps> = ({ project }) => {
  const startDate = new Date(project.startDate);
  const endDate = project.endDate ? new Date(project.endDate) : undefined;
  return (
    <Card>
      <Link href={"/projects/" + project.slug} passHref>
        <a className={style.cardLink}>
          <CardHeader>
            <h5>
              {project.title} <Badge color="primary">Project</Badge>
            </h5>
            <p className={dateClassName}>
              <DateInterval
                formatStyle="d MMM yyyy"
                startDate={startDate}
                endDate={endDate}
              />
            </p>
          </CardHeader>
          <CardBody>
            <p className="lead">{project.description} </p>
            <small className="text-muted">{project.excerpt!!}</small>
          </CardBody>
        </a>
      </Link>
      <CardFooter>
        <TagList tags={project.tags} limit={7} link />
      </CardFooter>
    </Card>
  );
};

type SiteObjectDisplayProps = {
  object: SiteObject;
};

const SiteObjectDisplay: FC<SiteObjectDisplayProps> = ({ object }) => {
  switch (object.type) {
    case "b":
      return (
        <BlogPostDisplay
          post={convertBlogPostToObjectDate(object as BlogPostMeta<string>)}
        />
      );
    case "p":
      return (
        <ProjectDisplay
          project={convertProjectToObjectDate(object as ProjectMeta<string>)}
        />
      );
    default:
      console.error("Empty type for object", object);
      throw new Error("Empty type");
  }
};

export type TagPageProps = {
  slug: string;
  related: string[];
  objects: SiteObject[];
};

const TagDetailTemplate: FC<TagPageProps> = ({ slug, related, objects }) => {
  const tag = useTagTable().get(slug);

  return (
    <Layout>
      <SEO title={tag.name!} description={`Items related to ${tag.name}`} />
      <Container tag="article">
        <header style={{ marginTop: 20 }}>
          <h1>
            Items related to <TagBadge tag={tag} />
          </h1>
        </header>

        {related.length > 0 ? (
          <section>
            <h4>Similar Tags</h4>
            <p>
              <TagList tags={related} link />
            </p>
          </section>
        ) : null}

        <section style={{ paddingBottom: 30 }}>
          <Masonry
            options={{
              transitionDuration: 0,
            }}
          >
            {objects.map((object, i) => (
              <Col xs="12" md="6" lg="4" key={i} style={{ paddingBottom: 30 }}>
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
