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
              World
            </a>{" "}
            <a href="https://en.wikipedia.org/wiki/Burlingame_High_School_(California)#Robotics">
              Champion
            </a>
            <a href="https://www.businesswire.com/news/home/20190420005006/en/Youth-Robotics-Teams-Inspire-Record-Crowds-FIRST%C2%AE">
              ship
            </a>
            -
            <a href="https://www.smdailyjournal.com/news/local/burlingame-high-school-claims-robotics-crown/article_8a3bb226-6895-11e9-9d1a-9b53ee5976f3.html">
              Winning
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
      <h6>FRC:</h6>
      <ul>
        <li>
          Built game element detection system running on a Nvidia Jetson using
          OpenCV and neural network. Used to assist driver in robot alignment.
        </li>
        <li>Researched advanced motion and path planning algorithms.</li>
      </ul>
      <h6>FTC:</h6>
      <ul>
        <li>Architected autonomous operation period.</li>
        <li>
          Designed a system to allow for chaining commands, making the
          autonomous behavior more predictable.
        </li>
      </ul>
    </article>
  );
}

function FabTimeArticle() {
  return (
    <article>
      <ArticleHeader experience={experiences.get("fabtime")}>
        Charting software for semiconductor fabricators
      </ArticleHeader>
      <ul>
        <li>
          Designed features to improve user experience and streamline fab cycles
        </li>
        <li>Implemented main site newsletter archive system</li>
        <li>Researched ways to migrate code to ASP.NET Core</li>
        <li>
          Pinpointed and eliminated bugs in various languages, including C,
          VBScript, JS, and SQL
        </li>
      </ul>
    </article>
  );
}

function GoodRippleArticle() {
  return (
    <article>
      <ArticleHeader experience={experiences.get("goodripple")}></ArticleHeader>
      <ul>
        <li>
          Integrated with the Slack API to add new features to the chatbot
        </li>
        <li>Interfaced with the MySQL database</li>
      </ul>
    </article>
  );
}

export function ExperienceSection() {
  return (
    <Container className={style.workExperience} tag="section">
      <h2 className={style.sectionHeading}>Work Experience</h2>
      <hr />
      <FabTimeArticle />
      <hr />
      <IronPanthersArticle />
      <hr />
      <GoodRippleArticle />
    </Container>
  );
}
