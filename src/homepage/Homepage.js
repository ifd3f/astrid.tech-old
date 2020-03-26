import React from "react";
import { Container } from "reactstrap";
import "../bootstrap.css";
import { SkillsSection } from "./Skills";
import { Heading } from "./Heading";

function Homepage() {
  return (
    <Container fluid>
      <Heading/>

      <Container>
        <div className="row">
          <div className="col-lg-8 col-md-7 col-sm-6">
            <p className="lead">
              Computer Engineering Student at Cal Poly
              <br />
              Software Engineering Intern at FabTime Inc.
            </p>
            <article>
              <p>
              </p>
              <p>

              </p>
            </article>
          </div>
        </div>
        <SkillsSection />
      </Container>
      
      <footer style={{ background: "#202030" }}>
        <div className="row">
          <div className="col-lg-12">
            <p>Made with ☕ and 🧙‍♀️ by Astrid Augusta Yu.</p>
            <p>
              Based on
              <a href="https://getbootstrap.com/" rel="nofollow">
                Bootstrap
              </a>
              . Icons from
              <a href="https://fontawesome.com/" rel="nofollow">
                Font Awesome
              </a>
              . Web fonts from
              <a href="https://fonts.google.com/" rel="nofollow">
                Google
              </a>
              .
            </p>
          </div>
        </div>
      </footer>
  </Container>
  );
}

export default Homepage;
