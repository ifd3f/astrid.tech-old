import Fuse from "fuse.js";
import { InferGetStaticPropsType } from "next";
import { useRouter } from "next/router";
import { FC, useEffect, useState } from "react";
import { Container } from "reactstrap";
import Layout from "../components/layout";
import SEO from "../components/seo";
import { getStaticPaths as projectPaths } from "./projects/[slug]";
import { getStaticPaths as tagPaths } from "./t/[slug]";
import { getStaticPaths as blogPaths } from "./[year]/[month]/[day]/[ordinal]/[slug]";

async function getAllPaths() {
  function uniques(arr: string[]) {
    var a = [];
    for (var i = 0, l = arr.length; i < l; i++)
      if (a.indexOf(arr[i]) === -1 && arr[i] !== "") a.push(arr[i]);
    return a;
  }

  return uniques(
    (
      await Promise.all(
        [blogPaths, tagPaths, projectPaths].map((x) => x.getStringPaths!())
      )
    )
      .flat()
      .concat(["/projects", "/about", "/", "/latest", "/licenses", "/privacy"])
  );
}

export const getStaticProps = async () => {
  const paths = await getAllPaths();
  const index = Fuse.createIndex(["."], paths).toJSON();

  return { props: { paths, index } };
};

type NotFoundPageContentsProps = {
  suggestions: string[];
  bugsURL: string;
};

const NotFoundPageContents: FC<NotFoundPageContentsProps> = ({
  suggestions,
  bugsURL,
}) => {
  const [first, ...rest] = suggestions;
  return (
    <div>
      <p style={{ fontSize: 20 }}>
        Perhaps you meant to go to{" "}
        <strong>
          <a href={first}>{first}</a>
        </strong>
        ? Or, maybe one of...
      </p>
      <ul>
        {rest.map((path) => (
          <li key={path}>
            <a href={path}>{path}</a>
          </li>
        ))}
      </ul>
      <p>
        Maybe I brought you here on accident! In that event, please{" "}
        <a href={bugsURL}>file a bug report on my GitHub issues page</a> and I
        will fix it as soon as possible!
      </p>
    </div>
  );
};

const NotFoundPage: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  paths,
  index,
}) => {
  const [suggestions, setSuggestions] = useState(null as string[] | null);
  const router = useRouter();

  useEffect(() => {
    const fuse = new Fuse(
      paths,
      {
        threshold: 1,
        findAllMatches: true,
      },
      Fuse.parseIndex(index)
    );
    const results = fuse.search(router.asPath);
    setSuggestions(results.slice(0, 8).map((x) => x.item));
  }, [router.asPath]);

  return (
    <Layout>
      <Container
        style={{
          padding: 20,
          height: "70vh",
        }}
        tag="main"
      >
        <h1>404 Not Found</h1>
        <SEO
          title="404 Not Found"
          description="This page doesn't exist! Panic!"
        />
        {suggestions ? (
          <NotFoundPageContents
            suggestions={suggestions}
            bugsURL="https://github.com/astridyu/astrid.tech/issues"
          />
        ) : null}
      </Container>
    </Layout>
  );
};

export default NotFoundPage;
