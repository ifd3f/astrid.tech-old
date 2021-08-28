import { format } from "date-fns";
import React, { FC, ReactNode } from "react";
import { FaTrophy } from "react-icons/fa";
import { Badge } from "reactstrap";
import { WorkExperience } from "../../types/types";
import { TagList } from "../tags/tag";
import style from "./style.module.scss";
import { HomepageSection } from "./util";

const IronPanthersTagline = () => {
  return (
    <p style={{ display: "inline-block", marginBottom: 5, marginRight: 5 }}>
      <FaTrophy />{" "}
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
      Robotics Team -{" "}
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
    </p>
  );
};
type ArticleProps = {
  experience: WorkExperience;
  tagline?: ReactNode;
};

const Article: FC<ArticleProps> = ({ experience, tagline: _tagline }) => {
  let tagline: ReactNode;
  if (_tagline != null) {
    tagline = _tagline;
  } else if (experience.summary) {
    tagline = <p>{experience.summary}</p>;
  } else {
    tagline = null;
  }

  const start = format(new Date(experience.startDate), "MMM yyyy");
  const durationText =
    experience.endDate == null
      ? start
      : `${start} to ${format(new Date(experience.endDate), "MMM yyyy")}`;

  return (
    <article>
      <div>
        <div className={style.mainHeader}>
          <h4 className={style.positionTitle}>
            {experience.position} at{" "}
            <a href={experience.website}>{experience.organization}</a>
          </h4>
          <div className={style.durationTitleWrapper}>
            <p className={style.durationTitle}>{durationText}</p>
          </div>
        </div>
      </div>

      <div>
        <TagList tags={experience.tags} />
      </div>

      <div>
        <ul>
          {experience.highlights.map((h) => (
            <li key={h}>{h}</li>
          ))}
        </ul>
      </div>
    </article>
  );
};

export function ExperienceSection() {
  const fb = require("../../data/objs/work/facebook");
  const mv = require("../../data/objs/work/micro-vu");
  const ft = require("../../data/objs/work/fabtime");

  return (
    <HomepageSection>
      <h2 className={style.sectionHeading}>Work Experience</h2>
      <hr />
      <Article experience={fb} />
      <hr />
      <Article experience={mv} />
      <hr />
      <Article experience={ft} />
    </HomepageSection>
  );
}
