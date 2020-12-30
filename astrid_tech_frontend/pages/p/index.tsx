import { FC } from "react";
import ProjectDetailPage from "../../components/projects/project-detail";
import { ProjectsIndex } from "../../components/projects/projects";
import { GetStaticProps, InferGetStaticPropsType } from "next";
import { getProjectMetas } from "../../lib/cache";
import Fuse from "fuse.js";

export const getStaticProps: GetStaticProps = async () => {
  const projects = getProjectMetas();

  const keys = [
    "title",
    "slug",
    "internal.description",
    "tags.name",
    "tags.slug",
  ];
  const index = Fuse.createIndex(keys, projects).toJSON();
  return {
    props: {
      index,
      keys,
      projects: projects.map(({ object }) => object),
    },
  };
};

const Page: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  index,
  keys,
  projects,
}) => {
  return (
    <ProjectsIndex projects={projects} fuseIndex={index} fuseKeys={keys} />
  );
};

export default Page;
