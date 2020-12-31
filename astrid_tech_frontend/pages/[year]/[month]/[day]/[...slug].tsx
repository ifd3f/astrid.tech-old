import { GetStaticPaths, GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import BlogPostPage from "../../../../components/blog/blog";
import { getBlogPost, getBlogPostSlugs, Path } from "../../../../lib/cache";
import { renderMarkdown } from "../../../../lib/markdown";
import { BlogPost, convertBlogPostToObjectDate } from "../../../../types/types";

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

  const content = await renderMarkdown(post.content, post.assetRoot);

  return {
    props: { post: { ...post, content: content } },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ post }) => {
  const p = post as BlogPost<string>;
  return (
    <BlogPostPage post={convertBlogPostToObjectDate(p) as BlogPost<Date>} />
  );
};

export default Post;
