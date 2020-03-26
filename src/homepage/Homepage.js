import React from "react";
import { Tooltip, Row, Container, Jumbotron, Col } from "reactstrap";
import "../bootstrap.css";
import { ProgrammingYears, HardwareYears } from "./YearsSince";
import { SkillsSection } from "./Skills";
import imgMyFace from "../assets/my-face.jpg"
import imgPcbWhite from "../assets/icon-pcb-white.png"
import { GiCircuitry } from "react-icons/gi"
import { BsCodeSlash } from "react-icons/bs"

function Homepage() {
  return (
    <Container fluid>
      <Jumbotron fluid>
        <Row>
          <Col className="text-right" md={5}>
            <p className="lead">My name is</p>
            <h1 className="display-1">ASTRID<br/>YU</h1>
          </Col>
          <Col md={2}>
            <img className="img-fluid" src={imgMyFace} />
          </Col>
          <Col className="text-left" md={5}>
            <p className="lead">I write</p>
            <h2 className="display-4">
              SOFTWARE <BsCodeSlash id="HeaderCodeIcon" />
            </h2>
            <p className="lead">and design</p>
            <h2 className="display-4">
              CIRCUITS <GiCircuitry/>
            </h2>
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
