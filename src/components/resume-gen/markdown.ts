import moment from "moment"
import { Education, Project, WorkExperience } from "../../types"
import {
  Resume,
  SECTION_EDUCATION,
  SECTION_PROJECTS,
  SECTION_WORK,
} from "./types"

function date(date?: Date | string | null) {
  if (typeof date == "string") {
    return date
  }
  return date ? moment(date).format("MMM YYYY") : "Present"
}

function h(levels: number) {
  return "#".repeat(levels) + " "
}

function renderExperience(levels: number, experience: WorkExperience) {
  const heading =
    h(levels) + experience.position + " @ " + experience.organization
  const subtitle =
    date(experience.startDate) +
    " - " +
    date(experience.endDate) +
    " / " +
    experience.location
  const skills = "_" + experience.tags.map(t => t.name).join(", ") + "_"
  const highlights = experience.highlights.map(h => " - " + h).join("\n")

  return [heading, subtitle, skills, highlights].join("\n\n")
}

function renderEducation(levels: number, education: Education) {
  const heading = h(levels) + education.degree + " @ " + education.name
  let subtitle = date(education.startDate) + " - " + date(education.endDate)
  if (education.gpa) {
    subtitle += " / " + education.gpa
  }

  return heading + "\n\n" + subtitle
}

function renderProject(levels: number, project: Project) {
  const heading = h(levels) + project.title
  const skills = "_" + project.tags.map(t => t.name).join(", ") + "_"

  return [heading, project.description, skills].join("\n\n")
}

export const generateMarkdownResume = (resume: Resume, order: string) => {
  let output = [
    h(1) + "Astrid Yu",
    resume.email + (resume.phone ? ` | ${resume.phone}` : ``),
    resume.address ? resume.address : null,
    `https://astrid.tech | https://github.com/Plenglin | https://linkedin.com/in/astrid-a-yu`,
  ]
    .filter(l => l)
    .join("\n\n")

  for (const sectionType of order) {
    switch (sectionType) {
      case SECTION_WORK:
        output += "\n\n" + h(2) + "Experience" + "\n\n"
        output += resume.work.map(w => renderExperience(3, w)).join("\n\n")
        break
      case SECTION_EDUCATION:
        output += "\n\n" + h(2) + "Education" + "\n\n"
        output += resume.schools.map(s => renderEducation(3, s)).join("\n\n")
        break
      case SECTION_PROJECTS:
        output += "\n\n" + h(2) + "Projects" + "\n\n"
        output += resume.projects.map(p => renderProject(3, p)).join("\n\n")
        break
    }
  }

  return output
}
