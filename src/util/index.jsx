import React, { useState } from "react";
import { Badge, UncontrolledTooltip, Tooltip } from "reactstrap";

import { resolveSkills } from "../db";

import style from "./util.module.css";
import { Link } from "react-router-dom";
import { Interval, MultiInterval } from "./interval";
import { MarkdownRender, MarkdownRenderAsync } from "./markdown";

var id = 0;
export function getUniqueId() {
  return id++;
}

export function SkillBadge({ skill, link = null }) {
  const { name, category } = skill;
  const [badgeId] = useState(`badge-${getUniqueId()}`);
  return (
    <>
      <Badge
        id={badgeId}
        style={{
          backgroundColor: category.color,
          marginRight: 2,
          marginLeft: 2,
        }}
        tag={Link}
        to={link ? link : undefined}
      >
        {name}
      </Badge>
    </>
  );
}

function SkillsList({ skills }) {
  return (
    <div>
      <p className={style.skillsList}>
        {resolveSkills(skills)
          .filter((skill) => skill.shown)
          .map(({ skill }) => {
            return <SkillBadge key={skill.id} skill={skill} />;
          })}
      </p>
    </div>
  );
}

export {
  SkillsList,
  Interval,
  MultiInterval,
  MarkdownRender,
  MarkdownRenderAsync,
};
