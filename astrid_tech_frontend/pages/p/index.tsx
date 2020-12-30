import { FC } from "react";
import ProjectDetailPage from "../../components/projects/project-detail";
import { ProjectsIndex } from "../../components/projects/projects";

const Page: FC = () => {
  return <ProjectsIndex projects={[]} fuseIndex={[]} />;
};

export default Page;
