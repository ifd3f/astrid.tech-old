import SEO from "components/seo";
import Link from "next/link";
import { join } from "path";
import React, { createContext, FC, useContext } from "react";
import path from "path";
import {
  BsArrowLeft,
  BsCodeSlash,
  BsLink,
  BsQuestionCircle,
} from "react-icons/bs";
import { FaCalendar, FaEnvelope, FaGithub } from "react-icons/fa";
import { Container } from "reactstrap";
import { ProjectLink } from "../../lib/cache";
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
  SidebarGroup,
  StatusGroup,
  TagsGroup,
} from "../layout";
import { StatusBadge } from "./project-card";
import style from "./project-detail.module.scss";
import { resolveAssetURL } from "../../lib/cache/assets";

type UsesProject = {
  project: Project<Date>;
};

function SourceCodeURLDisplay({ url }: { url: string }) {
  const info = new URL(url);
  if (info.hostname.endsWith("github.com")) {
    return (
      <a href={url}>
        <FaGithub title="GitHub" /> Github
      </a>
    );
  }
  if (info.protocol == "mailto:") {
    return (
      <a href={url}>
        <FaEnvelope /> {info.pathname}
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
        <StatusBadge status={project.status} />
      </InfoRow>
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

const RelatedProjectsGroup: FC<{ similar: ProjectLink[] }> = ({ similar }) => {
  const list = (
    <ul>
      {similar.map(({ slug, title }) => (
        <li key={slug}>
          <Link href={slug}>
            <a href={title}>{title}</a>
          </Link>
        </li>
      ))}
    </ul>
  );

  return (
    <SidebarGroup>
      <h2>Similar Projects</h2>
      {similar.length == 0 ? <p>N/A</p> : list}
    </SidebarGroup>
  );
};

type ProjectContextData = {
  project: Project;
};

const ProjectContext = createContext<ProjectContextData>(
  {} as ProjectContextData
);

export type ProjectDetailProps = UsesProject & {
  similar: ProjectLink[];
};

const ProjectDetailPage: FC<ProjectDetailProps> = ({ project, similar }) => {
  const slug = join("projects", project.slug);
  const url = join(process.env.publicRoot!, slug);
  const thumbnail = project.thumbnail
    ? path.join(
        "https://astrid.tech",
        resolveAssetURL(project.assetRoot, project.thumbnail)
      )
    : undefined;

  console.log("pdp thumb=", thumbnail);

  return (
    <ProjectContext.Provider value={{ project }}>
      <Layout currentLocation="projects">
        <LongformLayout
          title={project.title}
          description={project.description}
          thumbnail={thumbnail}
          descriptionRaw={project.description ?? "A project made by Astrid Yu"}
          headingColor={getHSLString(getPersistentColor(slug))}
          above={
            <Link href="/p">
              <a className={style.backToProjects}>
                <BsArrowLeft /> Back to Projects
              </a>
            </Link>
          }
          url={url}
          sidebar={
            <>
              <ProjectStatusGroup />
              <TagsGroup tags={project.tags} />
              {/* TODO <BlogPostsGroup /> */}
              <RelatedProjectsGroup similar={similar} />
            </>
          }
        >
          <ContentDisplay>{project.content}</ContentDisplay>
        </LongformLayout>
        <Container>
          <section id="comments">
            <h2>Comments</h2>
            <CommentSection slug={slug} />
          </section>
        </Container>
      </Layout>
    </ProjectContext.Provider>
  );
};

export default ProjectDetailPage;
