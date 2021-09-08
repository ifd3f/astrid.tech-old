import { FC } from "react";
import { Col, Container, Row } from "reactstrap";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import { LongCard } from "../components/card";
import styles from "../styles/blog.module.scss";
import Link from "next/link";

/**
 * Force this page to get exported
 */
export async function getStaticProps() {
  return { props: {} };
}

function Bio() {
  return (
    <article>
      <p>
        Hey, I'm Astrid and I'm a programmer. I got into coding when I was
        around 12, back when I played a ton of Minecraft. There was this mod for
        the game called{" "}
        <a href="http://www.computercraft.info/">ComputerCraft</a> that added
        computers to the game. I thought that was really cool, so I ended up
        teaching myself Lua to program those computers.
      </p>
      <p>
        Later on, I started branching out into more and more languages,
        technologies, and projects, and I eventually ended up with the
        almost-decade-long mess that you can see on the{" "}
        <Link href="/projects">projects</Link>. I wanted a place to share what
        I've made, so I created this website!
      </p>
    </article>
  );
}

const About: FC = (props) => {
  return (
    <Layout {...props} currentLocation="about">
      <SEO
        title="About"
        description="Information about the not-particularly-illustrious person known as Astrid Yu."
      />
      <Container className={styles.blogContentContainer}>
        <h1>About me</h1>
        <Row>
          <Col md={5}>
            <LongCard />
          </Col>
          <Col>
            <Bio />
          </Col>
        </Row>
      </Container>
    </Layout>
  );
};

export default About;
