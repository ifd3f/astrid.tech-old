import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import remark from "remark";
import html from "remark-html";
import ProjectDetailPage from "../../components/projects/project-detail";
import { getProject, getProjectSlugs } from "../../lib/cache";

export const getStaticPaths: GetStaticPaths = async () => {
  console.log(getProjectSlugs());
  return {
    paths: getProjectSlugs().map((slug) => ({ params: { slug } })),
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const { object: project, assetRoot } = getProject(params!!.slug as string);

  const content = (
    await remark().use(html).process(project.content)
  ).toString();

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
