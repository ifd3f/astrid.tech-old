import { createElement, CSSProperties, FC, ReactNode } from "react";
import { Col, Container, Row } from "reactstrap";

type HomepageSectionProps = {
  children: ReactNode;
  tag?: string;
  className?: string;
  style?: CSSProperties;
};

export const HomepageSection: FC<HomepageSectionProps> = ({
  children,
  tag = "section",
  style,
  className = "",
}) => {
  return createElement(
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
