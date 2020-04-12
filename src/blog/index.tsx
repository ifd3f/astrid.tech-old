import React from "react";
import { Container } from "reactstrap";
import md from "raw-loader!./entries/2020-04-12-test.md";

export default function Blog() {
  return (
    <>
      <Container tag="main">
        <h1>Blog</h1>
        <p>{md}</p>
      </Container>
    </>
  );
}
