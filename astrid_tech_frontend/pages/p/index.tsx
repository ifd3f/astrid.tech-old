import { FC } from "react";
import ProjectDetailPage from "../../components/projects/project-detail";
import { ProjectsIndex } from "../../components/projects/projects";
import { GetStaticProps } from "next";
import { getProjectMetas } from "../../lib/cache";

export const getStaticProps: GetStaticProps = async () => {
  const projects = getProjectMetas();

  return {
    props: {},
  };
};

const Page: FC = () => {
  return <ProjectsIndex projects={[]} fuseIndex={[]} />;
};

export default Page;
