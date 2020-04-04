import React from "react";
import { Container } from "reactstrap";
import { ExperienceSection } from "./Experience";
import FooterSection from "./Footer";
import HeadingSection from "./Heading";
import { SkillsSection } from "./Skills";
import ProjectsSection from "./Projects";

function Homepage() {
  return (
    <>
      <HeadingSection />
      <Container fluid>
        <ExperienceSection />
        <SkillsSection />
        <ProjectsSection />
        <FooterSection />
      </Container>
    </>
  );
}

export default Homepage;
