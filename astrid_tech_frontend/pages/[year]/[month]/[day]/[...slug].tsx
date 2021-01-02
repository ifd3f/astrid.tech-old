import { GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import BlogPostPage from "../../../../components/blog/blog";
import { getBlogPost, getBlogPostSlugs, Path } from "../../../../lib/cache";
import { renderMarkdown } from "../../../../lib/markdown";
import { wrappedStaticPaths } from "../../../../lib/pathcache";
import { BlogPost, convertBlogPostToObjectDate } from "../../../../types/types";

export const getStaticPaths = wrappedStaticPaths(
  __filename,
  async () => {
    return {
      paths: getBlogPostSlugs().map((params) => ({
        params,
      })),
      fallback: false,
    };
  },
  ({ year, month, day, slug }: Path) => `/${year}/${month}/${day}/${slug[0]}`
);

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
