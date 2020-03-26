import React from "react";
import { Media, Row, Container, Jumbotron, Col } from "reactstrap";
import "../bootstrap.css";
import { ProgrammingYears, HardwareYears } from "./YearsSince";
import { SkillsSection } from "./Skills";
import imgMyFace from "../assets/my-face.jpg"
import { GiCircuitry } from "react-icons/gi"
import { BsCodeSlash } from "react-icons/bs"

function Homepage() {
  return (
    <Container fluid>
      <Jumbotron fluid>
        <Col className="text-center">
          <p className="lead">My name is</p>
          <h1 className="display-1">ASTRID</h1>
          <p className="lead">and I'm a</p>
          <h1 className="display-1">HACKER</h1>
        </Col>
        <Row>
          <Col className="text-center" md={6}>
            <div className="d-inline-flex">
              <Col md="auto">
                <BsCodeSlash style={{fontSize: 100, height: "100%"}}/>
              </Col>
              <Col md="auto" className="text-left">
                <p>I've worked with</p>
                <h2>SOFTWARE</h2>
                <p>for <strong><ProgrammingYears/> years</strong></p>
              </Col>
            </div>
          </Col>
          <Col className="text-center" md={6}>
            <div className="d-inline-flex">
              <Col md="auto">
                <GiCircuitry style={{fontSize: 100, borderColor: "#FFFFFF", borderStyle: "solid"}}/>
              </Col>
              <Col md="auto" className="text-left">
                <p>as well as</p>
                <h2>HARDWARE</h2>
                <p>for <strong><HardwareYears/> years.</strong></p>
              </Col>
            </div>
          </Col>
        </Row>
      </Jumbotron>

      <Container>
        <div className="row">
          <div className="col-lg-8 col-md-7 col-sm-6">
            <p className="lead">
              Computer Engineering Student at Cal Poly
              <br />
              Software Engineering Intern at FabTime Inc.
            </p>
            <article>
              <p>
                Hey, I'm Astrid! I've been programming
                for <ProgrammingYears /> years and I've worked with hardware for <HardwareYears /> years. 
              </p>
              <p>

              </p>
            </article>
          </div>
        </div>

        <SkillsSection />
      </Container>
      <footer style={{ background: "#202030" }}>
        <div className="row">
          <div className="col-lg-12">
            <p>Made with ☕ and 🧙‍♀️ by Astrid Augusta Yu.</p>
            <p>
              Based on
              <a href="https://getbootstrap.com/" rel="nofollow">
                Bootstrap
              </a>
              . Icons from
              <a href="https://fontawesome.com/" rel="nofollow">
                Font Awesome
              </a>
              . Web fonts from
              <a href="https://fonts.google.com/" rel="nofollow">
                Google
              </a>
              .
            </p>
          </div>
        </div>
      </footer>
  </Container>
  );
}

export default Homepage;
