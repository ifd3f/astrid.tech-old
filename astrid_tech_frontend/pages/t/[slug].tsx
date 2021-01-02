import { GetStaticPaths, InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import TagPage from "../../components/tags/TagPage";
import { getTags } from "../../lib/cache/tag";

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
  return {
    props: { slug },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ slug }) => {
  return <TagPage slug={slug} objects={[]} related={[]} />;
};

export default Post;
