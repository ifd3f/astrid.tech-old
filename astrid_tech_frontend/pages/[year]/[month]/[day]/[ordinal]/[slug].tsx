import Redirect from "components/Redirect";
import { GetStaticProps, InferGetStaticPropsType } from "next";
import { FC } from "react";
import BlogPostPage from "components/blog/blog";
import { getBlogPost, getBlogPostSlugs, Path } from "lib/cache";
import { renderMarkdown } from "lib/markdown";
import { wrappedStaticPaths } from "lib/pathcache";
import { BlogPost, convertBlogPostToObjectDate } from "types/types";

type PageProps = { post: BlogPost<string> };

export const getStaticPaths = wrappedStaticPaths(
  __filename,
  async () => {
    const slugs = getBlogPostSlugs();
    return {
      paths: slugs.map((params) => ({ params })),
      fallback: false,
    };
  },
  ({ year, month, day, ordinal, slug }: Path) => {
    return `/${year}/${month}/${day}/${ordinal}/${slug}`;
  }
);

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const path = params!! as unknown as Path;
  const post = getBlogPost(path);

  const content = await renderMarkdown(post.content, post.assetRoot);

  return {
    props: {
      post: { ...post, content: content },
    } as PageProps,
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = (props) => {
  if (props.pageType == "redirect") {
    return <Redirect destination={props.to} />;
  }

  const p = props.post as BlogPost<string>;
  return (
    <BlogPostPage post={convertBlogPostToObjectDate(p) as BlogPost<Date>} />
  );
};

export default Post;
