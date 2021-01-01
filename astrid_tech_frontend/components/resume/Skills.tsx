import React, { FC, ReactNode } from "react";
import { BsStar, BsStarFill } from "react-icons/bs";
import Masonry from "react-masonry-component";
import { Col } from "reactstrap";
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
    level: number;
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
  level: number;
};

const SkillInfoDisplay: FC<SkillInfoDisplayProps> = ({ slug, level }) => {
  const tag = useTagTable().get(slug);
  return (
    <ALink href={`/t/${tag.slug}`}>
      <div
        className={styleSkills.skillRow}
        style={{ backgroundColor: tag.backgroundColor, color: tag.color }}
      >
        <p className={styleSkills.tagName}>{tag.name}</p>
        <p className={styleSkills.tagStars}>
          <Stars stars={level} />
        </p>
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
  <Col xs="12" sm="6" lg="4">
    <h3>{name}</h3>
    {skills
      .sort(({ level: a }, { level: b }) => b - a)
      .map(({ level, slug }) => (
        <SkillInfoDisplay level={level} slug={slug} key={slug} />
      ))}
  </Col>
);

export function SkillsSection() {
  const skills = require("../../data/objs/misc/skills") as SkillGroup[];
  return (
    <HomepageSection style={{ backgroundColor: "#55cdfc" }}>
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>Click on a tag to see related projects and blog posts!</p>
      </div>
      <Masonry options={{ transitionDuration: 0 }}>
        {skills.map((group) => (
          <SkillCategoryView key={group.name} category={group} />
        ))}
      </Masonry>
    </HomepageSection>
  );
}
