import React from "react";
import { Container } from "reactstrap";
import md from "./entries/2020-04-12-test.md";
import { MarkdownRenderAsync } from "../util";
import { posts } from "./entries";
import { BlogEntryRenderer } from "./entry";
import { Helmet } from "react-helmet";

export default function Blog() {
  return (
    <>
      <Helmet>
        <title>Astrid Yu - Blog</title>
      </Helmet>
      <Container tag="main">
        <h1>Blog</h1>
        <BlogEntryRenderer entry={posts[0]} />
      </Container>
    </>
  );
}
