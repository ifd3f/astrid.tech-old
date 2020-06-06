import * as React from "react";
import { Container } from "reactstrap";
import { Helmet } from "react-helmet";

export default function PortfolioPage() {
  return (
    <Container fluid tag="main">
      <Helmet>
        <title>Astrid Yu</title>
      </Helmet>
      <h1>Portfolio</h1>
    </Container>
  );
}
