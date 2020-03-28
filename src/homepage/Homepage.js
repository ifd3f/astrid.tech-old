import React from "react";
import { Container } from "reactstrap";
import { SkillsSection } from "./Skills";
import { HeadingSection } from "./Heading";
import { ExperienceSection } from "./Experience";
import { FooterSection } from "./Footer";

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
