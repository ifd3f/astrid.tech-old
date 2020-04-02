import React from "react";
import { BsCodeSlash } from "react-icons/bs";
import { GiCircuitry } from "react-icons/gi";
import { Col, Jumbotron, Row } from "reactstrap";
import { HardwareYears, ProgrammingYears } from "./YearsSince";

function HeadingSection() {
  return (
    <Jumbotron fluid>
      <Col className="text-center">
        <p className="lead">My name is</p>
        <h1 className="display-1">ASTRID</h1>
        <p className="lead">and I'm a</p>
        <h1 className="display-1">HACKER</h1>
      </Col>
      <Row>
        <Col className="text-center" lg={6}>
          <div className="d-inline-flex">
            <Col md="auto">
              <BsCodeSlash style={{ fontSize: 100, height: "100%" }} />
            </Col>
            <Col md="auto" className="text-left">
              <p>I've worked with</p>
              <h2>SOFTWARE</h2>
              <p>
                for{" "}
                <strong>
                  <ProgrammingYears /> years
                </strong>
              </p>
            </Col>
          </div>
        </Col>
        <Col className="text-center" lg={6}>
          <div className="d-inline-flex">
            <Col md="auto">
              <GiCircuitry
                style={{
                  fontSize: 100,
                  borderColor: "#FFFFFF",
                  borderStyle: "solid",
                }}
              />
            </Col>
            <Col md="auto" className="text-left">
              <p>as well as</p>
              <h2>HARDWARE</h2>
              <p>
                for{" "}
                <strong>
                  <HardwareYears /> years.
                </strong>
              </p>
            </Col>
          </div>
        </Col>
      </Row>
    </Jumbotron>
  );
}

export default HeadingSection;
