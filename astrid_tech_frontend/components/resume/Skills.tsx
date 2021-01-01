import React, { FC, ReactNode } from "react";
import { BsStar, BsStarFill } from "react-icons/bs";
import { Col, Row } from "reactstrap";
import { useTagTable } from "../tags/TagTableProvider";
import { ALink } from "../util/boilerplate";
import styleSkills from "./skills.module.scss";
import style from "./style.module.scss";
import { HomepageSection } from "./util";

type StarsProps = {
  stars: number;
};

export type SkillGroup = {
  name: string;
  skills: {
    slug: string;
  }[];
};

const Stars: FC<StarsProps> = ({ stars }) => {
  const out: ReactNode[] = Array(5);
  var i = 0;
  while (i < stars) {
    out.push(<BsStarFill key={i} />);
    i++;
  }
  while (i < 5) {
    out.push(<BsStar key={i} />);
    i++;
  }

  return <>{out}</>;
};

type SkillInfoDisplayProps = {
  slug: string;
};

const SkillInfoDisplay: FC<SkillInfoDisplayProps> = ({ slug }) => {
  const tag = useTagTable().get(slug);
  return (
    <ALink href={`/t/${tag.slug}`}>
      <div
        className={styleSkills.skillRow}
        style={{ backgroundColor: tag.backgroundColor, color: tag.color }}
      >
        <p className={styleSkills.tagName}>{tag.name}</p>
      </div>
    </ALink>
  );
};

type SkillCategoryViewProps = {
  category: SkillGroup;
};

const SkillCategoryView: FC<SkillCategoryViewProps> = ({
  category: { name, skills },
}) => (
  <>
    <div
      style={{
        height: 80,
        display: "flex",
        flexDirection: "column",
        justifyContent: "end",
      }}
    >
      <h3>{name}</h3>
    </div>
    {skills.map(({ slug }) => (
      <SkillInfoDisplay slug={slug} key={slug} />
    ))}
  </>
);

export function SkillsSection() {
  const skills = require("../../data/objs/misc/skills") as SkillGroup[];
  return (
    <HomepageSection style={{ backgroundColor: "#55cdfc" }}>
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>
          Click on <strong>bolded</strong> tags to see related projects and blog
          posts!
        </p>
      </div>
      <Row options={{ transitionDuration: 0 }}>
        {skills.map((group) => (
          <Col xs="6" sm="4" lg="3">
            <SkillCategoryView key={group.name} category={group} />
          </Col>
        ))}
      </Row>
    </HomepageSection>
  );
}
