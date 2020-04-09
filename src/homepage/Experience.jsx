import React from "react";
import { Badge, Container } from "reactstrap";
import { experiences } from "../db";
import { SkillsList } from "../util";
import style from "./experience.module.scss";

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
      <div className={style.mainHeader}>
        <h4 className={style.positionTitle}>
          {title} {preposition} <a href={website}>{company}</a>
        </h4>
        <div className={style.durationTitleWrapper}>
          <p className={style.durationTitle}>{timeDisplay}</p>
        </div>
      </div>
      {children ? <div className={style.tagline}>{children}</div> : ""}
      <SkillsList skills={skills} />
    </header>
  );
}

function IronPanthersArticle() {
  return (
    <article>
      <ArticleHeader experience={experiences.get("iron-panthers")}>
        <div style={{ display: "inline" }}>
          <p
            style={{ display: "inline-block", marginBottom: 5, marginRight: 5 }}
          >
            <a href="https://theburlingameb.org/1990/news/iron-panthers-win-world-championships/">
              World Championship-Winning
            </a>{" "}
            Robotics Team
          </p>
          <div style={{ display: "inline-block" }}>
            <Badge
              pill={true}
              color="primary"
              href="https://www.thebluealliance.com/team/5026"
            >
              FRC #5026
            </Badge>{" "}
            <Badge
              pill={true}
              color="warning"
              href="https://theorangealliance.org/teams/7316"
            >
              FTC #7316
            </Badge>
          </div>
        </div>
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
      <ArticleHeader experience={experiences.get("fabtime")}>
        Charting software for semiconductor fabricators
      </ArticleHeader>
    </article>
  );
}

function GoodRippleArticle() {
  return "";
}

export function ExperienceSection() {
  return (
    <Container className={style.workExperience} tag="section">
      <h2 className="section-header">Work Experience</h2>
      <hr />
      <FabTimeArticle />
      <hr />
      <GoodRippleArticle />
      <hr />
      <IronPanthersArticle />
    </Container>
  );
}
