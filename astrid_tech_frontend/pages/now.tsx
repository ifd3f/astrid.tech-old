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
      <p>This page was last updated on 2022-04-28.</p>
      <p>Some events that just happened are:</p>
      <ul>
        <li>
          I had a horrible winter quarter where I took 4 upper-division CS
          classes. Now I'm having a much more chill spring quarter with less
          classes.
        </li>
        <li>
          I went to the American Chemical Society conference in{" "}
          <Link href="/2022/03/24/0/acs-spring-2022/">San Diego</Link>.
        </li>
      </ul>
      <p>Some of my current side projects include:</p>
      <ul>
        <li>Writing more content for this website</li>
        <li>
          Setting up the 4th iteration of my{" "}
          <Link href="/projects/infrastructure">homelab</Link>
        </li>
        <li>My senior project, a weather app</li>
        <li>Various cybersecurity shenanigans</li>
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

  const seconds = Math.round((unixMs - BIRTH_MS) / 1000);

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
