import Link from "next/link";
import React, { FC, ReactNode } from "react";
import { BsStar, BsStarFill } from "react-icons/bs";
import { Col } from "reactstrap";
import { SkillGroup, Tag } from "../../types/types";
import styleSkills from "./skills.module.scss";
import style from "./style.module.scss";
import { HomepageSection } from "./util";

type StarsProps = {
  stars: number;
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
  tag: Tag;
  level: number;
};

const SkillInfoDisplay: FC<SkillInfoDisplayProps> = ({ tag, level }) => {
  return (
    <Link href={`/tags/${tag.slug}`}>
      <div
        className={styleSkills.skillRow}
        style={{ backgroundColor: tag.backgroundColor, color: tag.color }}
      >
        <p className={styleSkills.tagName}>{tag.name}</p>
        <p className={styleSkills.tagStars}>
          <Stars stars={level} />
        </p>
      </div>
    </Link>
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
      .map(({ level, tag }) => (
        <SkillInfoDisplay level={level} tag={tag} key={tag.slug} />
      ))}
  </Col>
);

type QueryData = {
  allSkillGroup: {
    nodes: SkillGroup[];
  };
};

export function SkillsSection() {
  return (
    <HomepageSection style={{ backgroundColor: "#55cdfc" }}>
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>Click on a tag to see related projects and blog posts!</p>
      </div>
      {/* <Masonry options={{ transitionDuration: 0 }}>
        {query.allSkillGroup.nodes.map((node) => (
          <SkillCategoryView key={node.id} category={node} />
        ))}
        </Masonry>*/}
    </HomepageSection>
  );
}
