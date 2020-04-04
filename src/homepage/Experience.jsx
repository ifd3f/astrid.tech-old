import React from "react";
import { Badge, Col, Button, CardLink, Container, Media } from "reactstrap";
import { imgFabTime } from "../assets";
import style from "./experience.module.css";
import { experiences, resolveSkills } from "../db";

function ArticleHeader({ experience, children }) {
  const {
    position: { title, preposition },
    company,
    website,
    timeDisplay,
    skills,
  } = experience;
  return (
    <header>
      <h4 className={style.positionTitle}>
        {title} {preposition} <a href={website}>{company}</a>
      </h4>
      <div className={style.durationTitleWrapper}>
        <p className={style.durationTitle}>{timeDisplay}</p>
      </div>
      {children ? <p className={style.tagline}>{children}</p> : ""}
      <SkillsList skills={skills} />
    </header>
  );
}

function SkillsList({ skills }) {
  return (
    <div>
      <p>
        {resolveSkills(skills)
          .filter((skill) => skill.shown)
          .map(({ skill }) => {
            const { name, category } = skill;
            console.log(category.color);
            return (
              <Badge key={skill.id} style={{ backgroundColor: category.color }}>
                {name}
              </Badge>
            );
          })}
      </p>
    </div>
  );
}

function IronPanthersArticle() {
  return (
    <article>
      <ArticleHeader experience={experiences.get("iron-panthers")}>
        <Badge
          pill={true}
          color="success"
          href="https://www.thebluealliance.com/event/2019cmptx"
        >
          2019 World Champions!
        </Badge>
        <Badge
          pill={true}
          color="primary"
          href="https://www.thebluealliance.com/team/5026"
        >
          FRC #5026
        </Badge>
        <Badge
          pill={true}
          color="warning"
          href="https://theorangealliance.org/teams/7316"
        >
          FTC #7316
        </Badge>
      </ArticleHeader>
      <p>
        On the FIRST Robotics Competition main team, I worked to build a vision
        processing system using a Nvidia Jetson as a co-processor to aid the
        driver in aligning the robot. This was achieved through a combination of
        OpenCV and a neural network.
      </p>
      <p>
        On the FIRST Tech Challenge subteam, I designed code to score points
        during the autonomous operation period. I created a versatile
        command-based system based on FRC's system to make the auto code more
        organized and logical, eliminating issues.
      </p>
    </article>
  );
}

function FabTimeArticle() {
  return (
    <article>
      <ArticleHeader experience={experiences.get("fabtime")}></ArticleHeader>
    </article>
  );
}

function GoodRippleArticle() {
  return "";
}

export function ExperienceSection() {
  return (
    <section>
      <Container>
        <h2 className="section-header">Work Experience</h2>
        <hr />
        <FabTimeArticle />
        <hr />
        <GoodRippleArticle />
        <hr />
        <IronPanthersArticle />
      </Container>
    </section>
  );
}
