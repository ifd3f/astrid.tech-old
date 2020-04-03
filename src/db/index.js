import * as projects from "./projects";

import skillsRaw from "./skills-data";

const categories = skillsRaw
  .map(({ id, name, color, children }) => ({
    id,
    name,
    color,
    children: children.map((s) => s.id),
  }))
  .reduce((map, category) => {
    map.set(category.id, category);
    return map;
  }, new Map());

const skills = skillsRaw
  .flatMap(({ id, children }) => children.map((x) => ({ ...x, category: id })))
  .map(({ id, name, skills, implied = [], impliedShown = [], category }) => ({
    id,
    name,
    category,
    skills,
    implied,
    impliedShown,
  }))
  .reduce((map, skill) => {
    map.set(skill.id, skill);
    return map;
  }, new Map());

for (let [, skill] of skills) {
  skill.implied = skill.implied
    .map((id2) => ({
      shown: false,
      skill: skills.get(id2),
    }))
    .concat(
      skill.impliedShown.map((id2) => ({
        shown: true,
        skill: skills.get(id2),
      }))
    );
  delete skill.impliedShown;
}

for (let [, category] of categories) {
  category.children = category.children.map((s) => skills.get(s));
}

console.log(skills);
console.log(categories);

export { categories, skills };
