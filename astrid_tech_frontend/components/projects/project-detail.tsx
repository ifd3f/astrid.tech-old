import Link from "next/link";
import { join } from "path";
import React, { createContext, FC, useContext } from "react";
import {
  BsArrowLeft,
  BsCodeSlash,
  BsLink,
  BsQuestionCircle,
} from "react-icons/bs";
import { FaCalendar, FaGithub } from "react-icons/fa";
import { Container } from "reactstrap";
import {
  formatDateInterval,
  getHSLString,
  getPersistentColor,
} from "../../lib/util";
import { Project } from "../../types/types";
import { CommentSection } from "../api/comments/CommentSection";
import { ContentDisplay } from "../content/ContentDisplay";
import {
  InfoRow,
  Layout,
  LongformLayout,
  StatusGroup,
  TagsGroup,
} from "../layout";
import style from "./project-detail.module.scss";

type Data = {
  project: Project;
};

type Context = {
  id: string;
};

function SourceCodeURLDisplay({ url }: { url: string }) {
  const info = new URL(url);
  if (info.hostname.endsWith("github.com")) {
    return (
      <a href={url}>
        <FaGithub title="GitHub" />
      </a>
    );
  }
  return <a href={url}>{url}</a>;
}

const ProjectStatusGroup = () => {
  const { project } = useContext(ProjectContext);
  return (
    <StatusGroup>
      <InfoRow name="Date" icon={<FaCalendar />}>
        {formatDateInterval("d MMM yyyy", project.startDate, project.endDate)}
      </InfoRow>
      {project.url ? (
        <InfoRow name="URL" icon={<BsLink />}>
          <a href={project.url}>{project.url}</a>
        </InfoRow>
      ) : null}
      <InfoRow name="Source" icon={<BsCodeSlash />}>
        {project.source.map((url) => (
          <p>
            <SourceCodeURLDisplay url={url} />
          </p>
        ))}
      </InfoRow>
      <InfoRow name="Status" icon={<BsQuestionCircle />}>
        {/* TODO Insert status here */}
      </InfoRow>
      {/* TODO Insert comment count here */}
    </StatusGroup>
  );
};

/*
const BlogPostsGroup = () => {
  const { project } = useContext(ProjectContext);
  const blogPosts = (project.childProjectTag.childTag.tagged.filter(
    (item) => item.__typename == "BlogPost"
  ) as BlogPost[]).sort(
    (a, b) => new Date(b.date).getTime() - new Date(a.date).getTime()
  );

  const list = (
    <ul>
      {blogPosts.map((post) => (
        <li key={post.slug}>
          <Link to={post.slug}>
            {moment(post.date).format("MMM YYYY")} - {post.title}
          </Link>
        </li>
      ))}
    </ul>
  );
  return (
    <SidebarGroup>
      <h2>Associated Blog Posts</h2>
      {blogPosts.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  );
};
*/

/*
const RelatedProjectsGroup = () => {
  const { project } = useContext(ProjectContext);

  const neighborNodes: BipartiteNode<Tag, Project>[] = project.tags.map(
    (tag) => ({
      id: tag.slug,
      neighbors: tag.tagged.map((project) => ({
        id: project.slug,
        neighbors: [],
        value: project as Project,
      })),
      value: tag,
    })
  );
  const orderedProjects = orderByResistorSimilarity(neighborNodes).filter(
    (other) => ![undefined, project.slug].includes(other.value.slug)
  );

  const list = (
    <ul>
      {orderedProjects.slice(0, 5).map(({ value: project }) => (
        <li key={project.slug}>
          <Link to={project.slug}>{project.title}</Link>
        </li>
      ))}
    </ul>
  );

  return (
    <SidebarGroup>
      <h2>Similar Projects</h2>
      {orderedProjects.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  );
};*/

type ProjectContextData = {
  project: Project;
};

const ProjectContext = createContext<ProjectContextData>(
  {} as ProjectContextData
);

export type ProjectDetailProps = {
  project: Project<Date>;
};

const ProjectDetailPage: FC<ProjectDetailProps> = ({ project }) => {
  const url = join(process.env.ROOT!!, "projects", project.slug);
  /*
  const thumbnail = data.project.thumbnail
    ? `${location.origin}${data.project.thumbnail.childImageSharp.fixed.src}`
    : undefined;*/

  return (
    <ProjectContext.Provider value={{ project }}>
      <Layout currentLocation="projects">
        <LongformLayout
          title={project.title}
          description={project.description}
          descriptionRaw={project.description ?? "A project made by Astrid Yu"}
          headingColor={getHSLString(getPersistentColor(project.slug))}
          above={
            <Link href="/p">
              <a className={style.backToProjects}>
                <BsArrowLeft /> Back to Projects
              </a>
            </Link>
          }
          thumbnail={undefined /* TODO */}
          url={url}
          sidebar={
            <>
              <ProjectStatusGroup />
              <TagsGroup tags={project.tags} />
              {/* TODO <BlogPostsGroup /> <RelatedProjectsGroup /> */}
            </>
          }
        >
          <ContentDisplay>{project.content}</ContentDisplay>
        </LongformLayout>
        <Container>
          <section id="comments">
            <h2>Comments</h2>
            <CommentSection slug={project.slug} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  );
};

export default ProjectDetailPage;
