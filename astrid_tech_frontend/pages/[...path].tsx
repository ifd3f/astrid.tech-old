import _ from "lodash";
import {
  GetServerSideProps,
  GetStaticProps,
  InferGetServerSidePropsType,
  InferGetStaticPropsType,
} from "next";
import { FC } from "react";
import BlogPostPage, { ClientBlogPost } from "../components/blog/blog";
import {
  getBlogPost,
  getBlogPostSlugs,
  getPathFromEntry,
  Path,
  resolveBlogPost,
} from "../lib/cache";
import { renderMarkdown } from "../lib/markdown";
import { wrappedStaticPaths } from "../lib/pathcache";
import { BlogPost, convertBlogPostToObjectDate } from "../types/types";

export const getServerSideProps: GetServerSideProps = async ({ params }) => {
  const {path} = params!!;
  const [year, month, day, ordinal, slugName] = path as string[];

  const posts = await resolveBlogPost(year, month, day, ordinal);
  console.debug("Resolved blog posts", posts);

  if (posts.length == 1) {
    const [post] = posts;
    const segments = getPathFromEntry(post);
    console.log("There is a single blog post", segments, post);

    if (!_.isEqual(segments, path)) {
      const destination = "/" + segments.join("/");
      console.log("Redirecting user to canonical URL", destination);

      return {
        redirect: {
          permanent: false,
          destination,
        },
      };
    }
    console.log("Rendering content");

    const fullPost = await getBlogPost(year, month, day, ordinal)

    const content = ""; // TODO

    return {
      props: {
        exact: true,
        post: {
          title: fullPost.title,
          contentHtml: content,
          path,
          tags: fullPost.tags,
          description: fullPost.description,
        } as ClientBlogPost,
      },
    };
  }

  return {
    props: {
      exact: false,
      posts,
    },
  };
};

const Post: FC<InferGetServerSidePropsType<typeof getServerSideProps>> = ({
  exact,
  post,
  posts,
}) => {
  if (exact) {
    return <p>TODO</p>; //<BlogPostPage post={post} />;
  }
  return <p>TODO multiple Posts</p>; // TODO
};

export default Post;
