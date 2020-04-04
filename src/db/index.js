import projectsRaw from "./projects";
import experiencesRaw from "./experience";
import skillsRaw from "./skills-data";
import { SkillsSection } from "../homepage/Skills";

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

const SKILLS = skillsRaw
  .flatMap(({ id, children }) => children.map((x) => ({ ...x, category: id })))
  .map((skill) => {
    if (skill.implied === undefined) {
      skill.implied = [];
    }
    skill.implied.push(skill.category);
    if (skill.impliedShown === undefined) {
      skill.impliedShown = [];
    }
    skill.projects = [];
    return skill;
  })
  // The categories themselves are skills.
  .concat(
    Array.from(categories.values()).map((category) => ({
      id: category.id,
      name: category.name,
      projects: [],
      implied: [],
      impliedShown: [],
      category: category.id,
    }))
  )
  .reduce((map, skill) => {
    map.set(skill.id, skill);
    return map;
  }, new Map());

const EXPERIENCES = experiencesRaw
  .map((experience) => {
    experience.skills = experience.skills.map((s) => SKILLS.get(s));
    return experience;
  })
  .reduce((map, experience) => {
    map.set(experience.id, experience);
    return map;
  }, new Map());

const projects = projectsRaw
  .map(
    ({
      title,
      desc,
      date,
      id,
      status,
      skills,
      url = null,
      img,
      source = null,
      info = null,
    }) => ({
      id,
      title,
      status,
      desc,
      date,
      skills: skills.map((sid) => {
        console.assert(
          SKILLS.has(sid),
          "Skill %s in project %s was not defined!",
          sid,
          id
        );
        return SKILLS.get(sid);
      }),
      url,
      img,
      source,
      info,
    })
  )
  .reduce((map, project) => {
    map.set(project.id, project);
    return map;
  }, new Map());

for (let [, skill] of SKILLS) {
  skill.category = categories.get(skill.category);
  skill.implied = skill.implied
    .map((id2) => ({
      shown: false,
      skill: SKILLS.get(id2),
    }))
    .concat(
      skill.impliedShown.map((id2) => ({
        shown: true,
        skill: SKILLS.get(id2),
      }))
    );
  delete skill.impliedShown;
}

for (let [, category] of categories) {
  category.children = category.children.map((s) => SKILLS.get(s));
}

for (let [, project] of projects) {
  for (let skill of project.skills) {
    skill.projects.push(project);
  }
}

function resolveSkills(skills) {
  const visited = new Map();
  const output = [];
  const stack = skills.map((s) => ({ shown: true, skill: s })).reverse();
  while (stack.length > 0) {
    const { shown, skill } = stack.pop();

    // Skip this one if we have visited it and we aren't turning on the shown state.
    if (visited.has(skill) && skill.shown && !visited.get(shown)) {
      continue;
    }

    output.push({ shown, skill });
    visited.set(skill, shown);
    if (shown) {
      for (const implied of skill.implied) {
        stack.push(implied);
      }
    }
  }
  return output;
}

export {
  resolveSkills,
  EXPERIENCES as experiences,
  categories,
  SKILLS as skills,
  projects,
};
