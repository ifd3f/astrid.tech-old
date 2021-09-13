import { FC, useState } from "react";
import { Col, Container, Row } from "reactstrap";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import styles from "../styles/blog.module.scss";
import Link from "next/link";
import { useInterval } from "react-interval-hook";

/**
 * Force this page to get exported
 */
export async function getStaticProps() {
  return { props: {} };
}

function Detail() {
  return (
    <>
      <p>Some events that just happened are:</p>
      <ul>
        <li>
          I just wrapped up my internship at Facebook! I should write a post
          about it at some point.
        </li>
        <li>Starting my 3rd school year at Cal Poly</li>
      </ul>
      <p>I'm currently trying to:</p>
      <ul>
        <li>
          Set up the 4th iteration of my{" "}
          <Link href="/projects/plebscale">homelab</Link>
        </li>
        <li>Write more content for this website</li>
        <li>Meet more people at school, since it's in-person</li>
        <li>Add general improvements to this website</li>
        <li>Find an idea for a senior project</li>
      </ul>
    </>
  );
}

function WhatIsThis() {
  return (
    <>
      <p className="text-muted">
        <strong>What is this page?</strong> This is a{" "}
        <a href="https://nownownow.com/about">Now page</a>. It's like an about
        page, except it's about what I'm doing right now.
      </p>
    </>
  );
}

function Age() {
  const BIRTH_MS = 976286100000;
  const [unixMs, setUnixMs] = useState(new Date().getTime());

  useInterval(() => {
    setUnixMs(new Date().getTime());
  });

  const seconds = Math.round((unixMs  - BIRTH_MS) / 1000);

  return <>{seconds} seconds</>;
}

const Now: FC = (props) => {
  return (
    <Layout {...props} currentLocation="now">
      <SEO
        title="About"
        description="Information about the not-particularly-illustrious person known as Astrid Yu."
      />
      <Container className={styles.blogContentContainer}>
        <h1>Now</h1>
        <WhatIsThis />
        <p>
          I am currently <Age /> old.
        </p>
        <Detail />
      </Container>
    </Layout>
  );
};

export default Now;
