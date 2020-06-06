import React from "react";
import { Container, Col, Row } from "reactstrap";
import styles from "./elevator.module.scss";

export default function ElevatorSection() {
  return (
    <Container tag="section">
      <div className={styles.elevatorBlock}>
        <p>
          Programmer by day, sleepy by night. I'm currently an intern at FabTime
          Inc. where I build software that tracks and optimizes the cycle times
          of silicon wafer fab plants. I'm studying computer engineering at Cal
          Poly, home of "Learn By Doing." When I'm not programming, you can
          probably catch me stir-frying another death dish or building a cool
          robot.
        </p>
      </div>
    </Container>
  );
}
