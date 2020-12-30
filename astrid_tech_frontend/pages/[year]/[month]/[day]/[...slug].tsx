import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import { getBlogPost, getBlogPostSlugs, Path } from "../../../../lib/cache";
import { BlogPost } from "../../../../types/types";

function pathToKey(path: Path) {
  const joined = path.slug.join(" ");
  return `${path.year}${path.month}${path.day}${joined}`;
}

export const getStaticPaths: GetStaticPaths = async () => {
  return {
    paths: getBlogPostSlugs().map((params) => ({
      params,
    })),
    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const post = getBlogPost(params!! as Path);
  console.log(post);
  return {
    props: { post },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ post }) => {
  const p = post as BlogPost<string>;
  return <p>{p.content}</p>;
};

export default Post;
