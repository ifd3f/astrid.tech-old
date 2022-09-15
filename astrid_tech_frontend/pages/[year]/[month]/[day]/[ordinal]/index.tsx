import Redirect from "components/Redirect";
import { GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import { getBlogPost, getBlogPostSlugs, Path } from "lib/cache";

type PageProps = { destination: string };

export const getStaticPaths = async () => {
  const slugs = getBlogPostSlugs();
  return {
    paths: slugs.map((params) => ({ params })),
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const path = params!! as unknown as Path;
  const post = getBlogPost(path);

  return {
    props: {
      destination: `/${path.year}/${path.month}/${path.day}/${path.ordinal}/${post.slug}`,
    } as PageProps,
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = (props) => {
  return <Redirect destination={props.destination} />;
};

export default Post;
