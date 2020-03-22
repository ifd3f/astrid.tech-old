import React from "react";
import { Container, Progress } from "reactstrap";
import "../bootstrap.css";
import ProgrammingYears from "./ProgrammingYears";
import { AnimatedSkillBar } from "./Skills";

function Homepage() {
  return (
    <Container>
      <header className="page-header" id="banner">
        <div className="row">
          <div className="col-lg-8 col-md-7 col-sm-6">
            <h1>Astrid Augusta Yu</h1>
            <p className="lead">
              Computer Engineering Student at Cal Poly
              <br />
              Software Engineering Intern at FabTime Inc.
            </p>
            <article>
              <p>
                Hey! I'm Astrid, and I've been programming for <ProgrammingYears/> years. I'm 
              </p>
            </article>
          </div>
          <div className="col-lg-4 col-md-5 col-sm-6">
            <div className="sponsor">
              <img className="img-fluid" src="/assets/avatar-2020-03-11.jpg" />
            </div>
          </div>
        </div>
      </header>

      <section>
        <div className="page-header">
          <h1>Skills</h1>
        </div>
        <div className="row">
          <div className="col-lg-6">
            <AnimatedSkillBar value={90} />
          </div>
        </div>
      </section>

      <footer id="footer">
        <div className="row">
          <div className="col-lg-12">
            <p>Made with 🧙‍♀️ by Astrid Augusta Yu.</p>
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
