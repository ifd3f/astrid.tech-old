import React from "react"
import { BsEnvelope } from "react-icons/bs"
import { FaLinkedin } from "react-icons/fa"
import { GiPhone } from "react-icons/gi"
import { GoMarkGithub } from "react-icons/go"
import { Col, Row } from "reactstrap"
import Layout from "../components/layout/layout"
import SEO from "../components/seo"
import {
  EducationSection,
  ExperienceSection,
  HeadingSection,
  SkillsSection,
} from "../components/_index/index"
import styles from "../components/_index/style.module.scss"
import { HomepageSection } from "../components/_index/util"
import "../scss/homepage.scss"

const Homepage = () => {
  return (
    <Layout>
      <SEO
        title="Astrid Yu"
        description="My name is Astrid Yu and I create software and other cool things. Welcome to my website!"
      />
      <div className={styles.homepageContainer}>
        <HeadingSection />
        <SkillsSection />
        <EducationSection />
        <ExperienceSection />
        <HomepageSection style={{ backgroundColor: "white" }}>
          <h2>Ways to get in touch with me</h2>
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
              <a href="https://github.com/Plenglin" rel="me">
                <GoMarkGithub title="GitHub" /> Follow me on GitHub
              </a>
            </Col>
            <Col xs="12" md="6" lg="3">
              <a href="https://linkedin.com/in/astrid-yu">
                <FaLinkedin title="LinkedIn" /> Connect on LinkedIn
              </a>
            </Col>
          </Row>
        </HomepageSection>
      </div>
    </Layout>
  )
}

export default Homepage
