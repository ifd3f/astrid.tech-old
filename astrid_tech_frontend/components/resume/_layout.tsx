import React, { CSSProperties, FC, ReactNode } from 'react';
import { Col, Container, Row } from 'reactstrap';

export const HomepageSection: FC<{
  children: ReactNode;
  tag?: string;
  className?: string;
  style?: CSSProperties;
}> = ({ children, tag = 'section', style, className = '' }) => {
  return React.createElement(
    tag,
    {
      className,
      style,
    },
    children
  );
};

export const Heading = () => {
  return (
    <HomepageSection>
      <Container fluid>
        <Row>
          <Col xs="12" lg="6">
            left
          </Col>
          <Col>right</Col>
        </Row>
      </Container>
    </HomepageSection>
  );
};
