import Head from "next/head";
import { GetStaticProps, InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import BlogPostPage from "../../components/blog/blog";
import { getBlogPost, getBlogPostSlugs, Path } from "../../lib/cache";
import { renderMarkdown } from "../../lib/markdown";
import { wrappedStaticPaths } from "../../lib/pathcache";
import { BlogPost, convertBlogPostToObjectDate } from "../../types/types";
import Layout from "components/layout";
import { Container } from "next/app";

function pathToOldSlug(path: Path) {
  const {
    year,
    month,
    day,
    slug: [slug],
  } = path;
  return `${year}-${month}-${day}-${slug}`;
}

function oldSlugToPath(oldSlug: string): Path | null {
  const pattern = /(\d{4})-(\d{2})-(\d{2})-(.+)/;
  const match = pattern.exec(oldSlug);
  if (!match) return null;
  const [, year, month, day, slug] = match;
  return { year, month, day, slug: [slug] };
}

export const getStaticPaths = async () => {
  const paths = getBlogPostSlugs();
  console.log(
    paths.map((p) => ({
      params: {
        oldSlug: pathToOldSlug(p),
      },
    }))
  );
  return {
    paths: paths.map((p) => ({
      params: {
        oldSlug: pathToOldSlug(p),
      },
    })),

    fallback: false,
  };
};

export const getStaticProps: GetStaticProps = async ({ params }) => {
  return {
    props: { path: oldSlugToPath(params!.oldSlug as string) },
  };
};

const Post: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({ path }) => {
  const {
    year,
    month,
    day,
    slug: [slug],
  } = path;
  const destination = `/${year}/${month}/${day}/${slug}`;
  return (
    <div>
      <Head>
        <meta httpEquiv="refresh" content={`0;url=${destination}`} />
      </Head>
      <p>
        This content has moved. If you haven't been redirected, please click{" "}
        <a href={destination}>here</a>.
      </p>
    </div>
  );
};

export default Post;
