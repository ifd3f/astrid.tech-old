import React from "react";
import { Container } from "reactstrap";
import md from "./entries/2020-04-12-test.md";
import { MarkdownRenderAsync } from "../util";
import { posts } from "./entries";
import { BlogEntryRenderer } from "./entry";

export default function Blog() {
  return (
    <>
      <Container tag="main">
        <h1>Blog</h1>
        <BlogEntryRenderer entry={posts[0]} />
      </Container>
    </>
  );
}
