import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import ProjectDetailPage from "../../components/projects/project-detail";
import { getProject, getProjectSlugs } from "../../lib/cache";
import { renderMarkdown } from "../../lib/markdown";

export const getStaticPaths: GetStaticPaths = async () => {
  console.log(getProjectSlugs());
  return {
    paths: getProjectSlugs().map((slug) => ({ params: { slug } })),
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const project = getProject(params!!.slug as string);

  const content = await renderMarkdown(project.content, project.assetRoot);

  return {
    props: { project: { ...project, content: content } },
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  project,
}) => {
  return <ProjectDetailPage project={project} />;
};

export default Page;
