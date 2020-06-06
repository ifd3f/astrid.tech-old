import React from "react";
import { Container } from "reactstrap";
import { ExperienceSection } from "./Experience";
import HeadingSection from "./Heading";
import { SkillsSection } from "./Skills";
import ProjectsSection from "./Projects";
import ElevatorSection from "./elevator";
import { Helmet } from "react-helmet";

export default function Homepage() {
  return (
    <main>
      <Helmet>
        <title>Astrid Yu</title>
      </Helmet>
      <HeadingSection />
      <ElevatorSection />
      <SkillsSection />
      <ExperienceSection />
      <ProjectsSection />
    </main>
  );
}
