import React from "react";
import { Container } from "reactstrap";
import { SkillsSection } from "./Skills";
import { HeadingSection } from "./Heading";
import { ExperienceSection } from "./Experience";

function Homepage() {
  return (
    <Container fluid>
      <HeadingSection/>
      <Container>
        <ExperienceSection/>
        <SkillsSection />
      </Container>
      
      <footer style={{ background: "#202030" }}>
        <div className="row">
          <div className="col-lg-12">
            <p>Made with <span role="img">☕ </span> and <span role="img">🧙‍♀️ </span> by Astrid Augusta Yu.</p>
          </div>
        </div>
      </footer>
  </Container>
  );
}

export default Homepage;
