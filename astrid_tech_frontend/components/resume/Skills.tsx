import React, { FC, ReactNode } from "react";
import { BsCaretRightFill, BsStar, BsStarFill } from "react-icons/bs";
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
  const link = !!tag.count;
  const inner = (
    <div
      className={styleSkills.skillRow}
      style={{ backgroundColor: tag.backgroundColor, color: tag.color }}
    >
      <p className={styleSkills.tagName}>
        {tag.name} {link ? <BsCaretRightFill name="caret" /> : null}
      </p>
    </div>
  );
  return link ? <ALink href={`/t/${tag.slug}`}>{inner}</ALink> : inner;
};

type SkillCategoryViewProps = {
  category: SkillGroup;
};

const SkillCategoryView: FC<SkillCategoryViewProps> = ({
  category: { name, skills },
}) => (
  <>
    <h3>{name}</h3>
    {skills.map(({ slug }) => (
      <SkillInfoDisplay slug={slug} key={slug} />
    ))}
  </>
);

export function SkillsSection() {
  const skills = [] as SkillGroup[];
  return (
    <HomepageSection style={{ backgroundColor: "#55cdfc" }}>
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>
          Click on tags with <BsCaretRightFill name="caret" /> to see related
          projects and blog posts!
        </p>
      </div>
      <Masonry options={{ transitionDuration: 0 }}>
        {skills.map((group) => (
          <Col key={group.name} xs="6" sm="4" lg="3">
            <SkillCategoryView key={group.name} category={group} />
          </Col>
        ))}
      </Masonry>
    </HomepageSection>
  );
}
