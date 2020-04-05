import React from "react";
import { Badge } from "reactstrap";

import { resolveSkills } from "../db";

import style from "./util.module.css";

function SkillsList({ skills }) {
  return (
    <div>
      <p className={style.skillsList}>
        {resolveSkills(skills)
          .filter((skill) => skill.shown)
          .map(({ skill }) => {
            const { name, category } = skill;
            console.log(category.color);
            return (
              <Badge
                key={skill.id}
                style={{
                  backgroundColor: category.color,
                  marginRight: 2,
                  marginLeft: 2,
                }}
              >
                {name}
              </Badge>
            );
          })}
      </p>
    </div>
  );
}

export { SkillsList };
