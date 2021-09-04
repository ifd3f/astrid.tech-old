import React, { ReactNode } from 'react';
import { FaDiceD20 } from 'react-icons/fa';
import { GiNoodles } from 'react-icons/gi';
import { Col, Row } from 'reactstrap';
import { HomepageSection } from './util';

function BigIcon({
  upper,
  children,
}: {
  upper: ReactNode;
  children: ReactNode;
}) {
  return (
    <Col xs="12" lg="4">
      <div>{upper}</div>
      <div>{children}</div>
    </Col>
  );
}

export function ElevatorSection() {
  return (
    <HomepageSection>
      <Row>
        <BigIcon upper={<>9 years</>}>of experience programming</BigIcon>
        <BigIcon upper={<>21 projects</> /*TODO*/}>
          tracked on this website
        </BigIcon>
        <BigIcon upper={<FaDiceD20 />}>Likes D&D</BigIcon>
        <BigIcon upper={<GiNoodles />}>Enjoys cooking</BigIcon>
      </Row>
    </HomepageSection>
  );
}
