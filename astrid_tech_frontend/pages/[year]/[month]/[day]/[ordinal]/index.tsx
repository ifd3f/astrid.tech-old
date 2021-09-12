import Redirect from "components/Redirect";
import { GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import { getBlogPost, getBlogPostSlugs, Path } from "lib/cache";
import { wrappedStaticPaths } from "lib/pathcache";
import { blogSlugToString } from "lib/util";

type PageProps = { destination: string };

export const getStaticPaths = wrappedStaticPaths(
  __filename,
  async () => {
    const slugs = getBlogPostSlugs();
    return {
      paths: slugs.map((params) => ({ params })),
      fallback: false,
    };
  },
  (path: Path) => {
    return blogSlugToString(path);
  }
);

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
