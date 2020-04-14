import React from "react";
import { Container } from "reactstrap";
import { ExperienceSection } from "./Experience";
import HeadingSection from "./Heading";
import { SkillsSection } from "./Skills";
import ProjectsSection from "./Projects";
import ElevatorSection from "./elevator";

export default function Homepage() {
  return (
    <main>
      <HeadingSection />
      <ElevatorSection />
      <ExperienceSection />
      <SkillsSection />
      <ProjectsSection />
    </main>
  );
}
