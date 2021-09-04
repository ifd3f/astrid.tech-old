import { ReactNode } from "react";
import { Col, Row } from "reactstrap";
import tStyles from "./testimonials.module.scss";
import { HomepageSection } from "./util";

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

export function TestmoninalSection() {
  return (
    <HomepageSection>
      <Row>
        <Col className={tStyles.testimonial}>
          <blockquote>
            "Astrid is a skilled troubleshooter and programmer and her strength
            is getting up to speed quickly as well as creating new functionality
            &mdash; none of these can be taught in an internship. I never saw
            her get stuck on a task or not have a proposed solution to a
            problem."
            <footer className="text-right">
              &mdash; <strong>Lara Nichols</strong>, Director of Engineering at{" "}
              <a href="https://fabtime.com">Fabtime Inc.</a>
            </footer>
          </blockquote>
        </Col>
      </Row>
    </HomepageSection>
  );
}
