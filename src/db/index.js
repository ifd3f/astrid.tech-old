import projectsRaw from "./projects";

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

const SKILLS = skillsRaw
  .flatMap(({ id, children }) => children.map((x) => ({ ...x, category: id })))
  .map(({ id, name, implied = [], impliedShown = [], category }) => ({
    id,
    name,
    category,
    implied: implied.concat([category]),
    impliedShown,
    projects: [],
  }))
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

export { categories, SKILLS as skills, projects };
