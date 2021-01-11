import { GetStaticProps, InferGetStaticPropsType } from "next";
import path from "path";
import { FC } from "react";
import ProjectDetailPage from "../../components/projects/project-detail";
import { getProject, getProjectSlugs } from "../../lib/cache";
import { getSimilarProjects } from "../../lib/cache/project";
import { renderMarkdown } from "../../lib/markdown";
import { wrappedStaticPaths } from "../../lib/pathcache";
import { convertProjectToObjectDate, Project } from "../../types/types";

export const getStaticPaths = wrappedStaticPaths(
  path.join(__dirname, __filename),
  async () => {
    return {
      paths: getProjectSlugs().map((slug) => ({ params: { slug } })),
      fallback: false,
    };
  },
  ({ slug }) => `/projects/${slug}`
);

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const project = getProject(params!!.slug as string);
  const similar = getSimilarProjects(project.id!!);
  const content = await renderMarkdown(project.content, project.assetRoot);
  return {
    props: {
      project: { ...project, content: content },
      similar,
    },
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  project,
  similar,
}) => {
  return (
    <ProjectDetailPage
      project={convertProjectToObjectDate(project) as Project<Date>}
      similar={similar}
    />
  );
};

export default Page;
