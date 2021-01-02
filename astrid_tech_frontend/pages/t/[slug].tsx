import { GetStaticPaths, InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import TagPage from "../../components/tags/TagPage";
import { getTagged, getTags } from "../../lib/cache/tag";

export const getStaticPaths: GetStaticPaths = async () => {
  return {
    paths: getTags().map((tag) => ({
      params: { slug: tag },
    })),
    fallback: false,
  };
};

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
