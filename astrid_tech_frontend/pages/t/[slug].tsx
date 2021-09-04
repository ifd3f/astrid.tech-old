import { InferGetStaticPropsType } from 'next';
import path from 'path';
import React, { FC } from 'react';
import TagPage from '../../components/tags/TagPage';
import { getTagged, getTags } from '../../lib/cache/tag';
import { wrappedStaticPaths } from '../../lib/pathcache';

export const getStaticPaths = wrappedStaticPaths(
  path.join(__dirname, __filename),
  async () => {
    return {
      paths: getTags()
        .filter((tag) => !tag.startsWith('/'))
        .map((tag) => ({
          params: { slug: tag },
        })),
      fallback: false,
    };
  },
  ({ slug }) => `/t/${slug}`
);

export const getStaticProps = async ({ params }: { params: any }) => {
  const slug = params.slug;
  const objects = await getTagged(slug, 100);
  return {
    props: { slug, objects },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  slug,
  objects,
}) => {
  return <TagPage slug={slug} objects={objects} related={[]} />;
};

export default Post;
