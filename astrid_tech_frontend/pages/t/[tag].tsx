import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";

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
  return {
    props: { post: { ...post, content: content } },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ post }) => {
  const p = post as BlogPost<string>;
  return <TagPage post={convertBlogPostToObjectDate(p) as BlogPost<Date>} />;
};

export default Post;
