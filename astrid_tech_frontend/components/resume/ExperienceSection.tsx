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

  const durationText =
    experience.endDate == null
      ? experience.startDate
      : `${experience.startDate} to ${experience.endDate}`;

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
        {tagline}
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

type QueryData = {
  fabtime: WorkExperience;
  microvu: WorkExperience;
  ironPanthers: WorkExperience;
};

export function ExperienceSection() {
  return (
    <HomepageSection color="#ddf2c4">
      {/* TODO <h2 className={style.sectionHeading}>Work Experience</h2>
      <hr />
      <Article experience={query.microvu} />
      <hr />
      <Article experience={query.fabtime} />
      <hr />
      <Article
        experience={query.ironPanthers}
        tagline={<IronPanthersTagline />}
      />
      */}
    </HomepageSection>
  );
}
