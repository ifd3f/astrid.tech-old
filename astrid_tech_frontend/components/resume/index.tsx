import { useEffect } from "react";
import { BsEnvelope } from "react-icons/bs";
import { FaLinkedin } from "react-icons/fa";
import { GiPhone } from "react-icons/gi";
import { GoMarkGithub } from "react-icons/go";
import { Col, Row } from "reactstrap";
import Layout from "../layout";
import SEO from "../seo";
import { ExperienceSection } from "./ExperienceSection";
import { HeadingSection } from "./Heading";
import { SkillsSection } from "./Skills";
import styles from "./style.module.scss";
import { HomepageSection } from "./_layout";

export const Page = ({}) => {
  useEffect(() => {
    console.log(
      "Do you like to use inspect element? Well, I do too! Please hire me :)"
    );
  }, []);

  return (
    <Layout currentLocation="resume">
      <SEO title="Astrid Yu" description="My digital resume" />
      <div className={styles.homepageContainer}>
        <HeadingSection />
        {/*<TestmoninalSection />*/}
        <SkillsSection />
        <ExperienceSection />
        {/*<ProjectsSection />*/}
        <HomepageSection style={{ backgroundColor: "white" }}>
          <h2>Get in touch with me!</h2>
          <hr />
          <Row className="text-center" style={{ fontSize: "16pt" }}>
            <Col xs="12" md="6" lg="3">
              <a href="mailto:astrid@astrid.tech" rel="me">
                <BsEnvelope title="Email" /> astrid@astrid.tech
              </a>
            </Col>
            <Col xs="12" md="6" lg="3">
              <a href="tel:+18052705368">
                <GiPhone title="Phone" /> ‪(805) 270-5368‬
              </a>
            </Col>
            <Col xs="12" md="6" lg="3">
              <a href="https://github.com/astralbijection" rel="me">
                <GoMarkGithub title="GitHub" /> Follow me on GitHub
              </a>
            </Col>
            <Col xs="12" md="6" lg="3">
              <a href="https://linkedin.com/in/astrid-yu" rel="me">
                <FaLinkedin title="LinkedIn" /> Connect on LinkedIn
              </a>
            </Col>
          </Row>
        </HomepageSection>
      </div>
    </Layout>
  );
};
