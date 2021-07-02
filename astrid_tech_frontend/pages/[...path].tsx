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
  const path = params!!.path!! as string[];

  // // Check if this is a permashortlink query
  // if (path.length == 1) {
  //   const [psl] = path;
  //   if (isNaN(parseInt(psl))) {
  //     // TODO (perma)shortlink handling   
  //   }
  // }

  // // Check if this is a year/month/day querry
  // if (path.length <= 3) {
  //   // TODO timespan page
  //   return {
  //     props: {
  //       pageType: 'entries',
  //       posts: [],  // TODO
  //     },
  //   };
  // }

  // Resolve as if this is a blog post

  const [year, month, day, ordinal, slugName] = path as string[];

  const posts = await resolveBlogPost(year, month, day, ordinal);
  console.debug("Resolved blog posts", posts);

  // We only found one blog post
  if (posts.length == 1) {
    const [post] = posts;
    const segments = getPathFromEntry(post);
    console.log("There is a single blog post", segments, post);

    // If this is not the canonical URL, redirect to the canonical URL
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
    
    // Render the content
    console.log("Rendering content");

    const fullPost = await getBlogPost(year, month, day, ordinal);

    const content = ""; // TODO

    return {
      props: {
        pageType: 'entry',
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
      pageType: 'entries',
      posts,
    },
  };
};

const Post: FC<InferGetServerSidePropsType<typeof getServerSideProps>> = ({
  pageType,
  post,
  posts,
}) => {
  if (pageType == 'post') {
    return <p>TODO</p>; //<BlogPostPage post={post} />;
  }
  return <p>TODO multiple Posts</p>; // TODO
};

export default Post;
