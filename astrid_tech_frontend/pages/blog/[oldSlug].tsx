import Redirect from "components/Redirect";
import { GetStaticProps, InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import { getBlogPostSlugs, Path } from "../../lib/cache";

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
  return <Redirect destination={`/${year}/${month}/${day}/${slug}`} />;
};

export default Post;
