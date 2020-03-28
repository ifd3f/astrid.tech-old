import React from "react";
import { Container } from "reactstrap";
import { ExperienceSection } from "./experience/Experience";
import { FooterSection } from "./Footer";
import { HeadingSection } from "./Heading";
import { SkillsSection } from "./Skills";

function Homepage() {
  return (
    <Container fluid>
      <HeadingSection />
      <Container>
        <ExperienceSection />
        <SkillsSection />
      </Container>
      <FooterSection />
    </Container>
  );
}

export default Homepage;
