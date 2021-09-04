import Fuse from 'fuse.js';
import { GetStaticProps, InferGetStaticPropsType } from 'next';
import { FC } from 'react';
import { ProjectsIndex } from '../../components/projects/projects';
import { getProjects } from '../../lib/cache';
import { withoutContent } from '../../lib/markdown';

export const getStaticProps: GetStaticProps = async () => {
  const projects = getProjects().map(withoutContent);

  const keys = [
    'title',
    'slug',
    'internal.description',
    'tags.name',
    'tags.slug',
  ];
  const index = Fuse.createIndex(keys, projects).toJSON();
  return {
    props: {
      index,
      keys,
      projects,
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
