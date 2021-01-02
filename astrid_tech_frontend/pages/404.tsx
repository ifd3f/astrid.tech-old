import { InferGetStaticPropsType } from "next";
import React, { FC, useEffect, useState } from "react";
import { Container } from "reactstrap";
import StringSimilarity from "string-similarity";
import Layout from "../components/layout";
import SEO from "../components/seo";
import { getStaticPaths as projectPaths } from "./projects/[slug]";
import { getStaticPaths as tagPaths } from "./t/[slug]";
import { getStaticPaths as blogPaths } from "./[year]/[month]/[day]/[...slug]";

async function getAllPaths() {
  return (
    await Promise.all(
      [blogPaths, tagPaths, projectPaths].map((x) => x.getStringPaths!())
    )
  )
    .flat()
    .concat(["/projects", "/about", "/", "/latest", "/licenses", "/privacy"]);
}

export const getStaticProps = async () => {
  const paths = await getAllPaths();

  return { props: { paths } };
};

const NotFoundPageContents: FC<{ suggestions: string[]; bugsURL: string }> = ({
  suggestions,
  bugsURL,
}) => (
  <div>
    <p style={{ fontSize: 20 }}>
      Perhaps you meant to go to{" "}
      <strong>
        <a href={suggestions[0]}>{suggestions[0]}</a>
      </strong>
      ? Or, maybe one of...
    </p>
    <ul>
      {suggestions.slice(1, 8).map((path) => (
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

const NotFoundPage: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  paths,
}) => {
  const [suggestions, setSuggestions] = useState(null as string[] | null);

  useEffect(() => {
    const results = StringSimilarity.findBestMatch(location.pathname, paths);
    setSuggestions(
      results.ratings
        .sort((a, b) => b.rating - a.rating)
        .map((result) => result.target)
        .slice(1, 8)
    );
  }, [suggestions]);

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
            bugsURL="https://github.com/Plenglin/astrid.tech/issues"
          />
        ) : null}
      </Container>
    </Layout>
  );
};

export default NotFoundPage;
