import React from "react";
import { Container } from "reactstrap";
import md from "./entries/2020-04-12-test.md";
import { MarkdownRenderAsync } from "../util";

export default function Blog() {
  return (
    <>
      <Container tag="main">
        <h1>Blog</h1>
        <MarkdownRenderAsync location={md} />
      </Container>
    </>
  );
}
