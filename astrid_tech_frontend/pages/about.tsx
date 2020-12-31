import React, { FC } from "react";
import { BsEnvelope } from "react-icons/bs";
import { GiPhone } from "react-icons/gi";
import { GoMarkGithub } from "react-icons/go";
import { Container } from "reactstrap";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import styles from "../scss/blog.module.scss";

type Data = {
  file: { childMarkdownRemark: { html: string } };
};

function Bio() {
  return (
    <div>
      <p>bio here</p>
    </div>
  );
}
const About: FC<{}> = (props) => {
  // TODO add linkedin when... well, you know
  return (
    <Layout {...props} currentLocation="about">
      <SEO
        title="About"
        description="Information about the not-particularly-illustrious person known as Astrid Yu."
      />
      <Container className={styles.blogContentContainer}>
        <Bio />
        <div>
          <ul>
            <li>
              <a href="mailto:hello@astrid.tech">
                <BsEnvelope title="Email" /> hello@astrid.tech
              </a>
            </li>
            <li>
              <a href="tel:+18052705368">
                <GiPhone title="Phone" /> ‪(805) 270-5368‬
              </a>
            </li>
            <li>
              <a href="https://github.com/Plenglin">
                <GoMarkGithub title="GitHub" /> Follow me on GitHub
              </a>
            </li>
          </ul>
        </div>
      </Container>
    </Layout>
  );
};

export default About;
