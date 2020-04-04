import React from "react";
import { BsCodeSlash } from "react-icons/bs";
import { GiCircuitry } from "react-icons/gi";
import { Col, Jumbotron, Row } from "reactstrap";
import { HardwareYears, ProgrammingYears } from "./YearsSince";
import { imgCoding } from "../assets";

import style from "./heading.module.css";

function IconInfoDisplay({ icon, children }) {
  return (
    <div className={style.subDisplayOuter}>
      <div className={style.subDisplay}>
        <div style={{ fontSize: 100, height: "100%" }}>{icon}</div>
        <div>{children}</div>
      </div>
    </div>
  );
}

function HeadingSection() {
  return (
    <div className={style.homepageTop}>
      <div className={style.headlineOuter}>
        <div className={style.headline}>
          <div>
            <p className="lead">My name is</p>
            <h1 className="display-1">ASTRID</h1>
            <p className="lead">and I'm a</p>
            <h1 className="display-1">HACKER</h1>
          </div>
        </div>
      </div>
      <div className={style.subRow}>
        <IconInfoDisplay icon={<BsCodeSlash />}>
          <p>I've worked with</p>
          <h2>SOFTWARE</h2>
          <p>
            for{" "}
            <strong>
              <ProgrammingYears /> years
            </strong>
          </p>
        </IconInfoDisplay>
        <IconInfoDisplay icon={<GiCircuitry />}>
          <p>as well as</p>
          <h2>HARDWARE</h2>
          <p>
            for{" "}
            <strong>
              <HardwareYears /> years.
            </strong>
          </p>
        </IconInfoDisplay>
      </div>
    </div>
  );
}

/*
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
          </Col>*/

export default HeadingSection;
