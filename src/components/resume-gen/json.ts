import { ResumeSchema } from "@kurone-kito/jsonresume-types"
import moment from "moment"
import { Education, Project, WorkExperience } from "../../types"
import { Resume } from "./types"

function date(date?: Date | string | null) {
  if (typeof date == "string") {
    return date
  }
  return date ? moment(date).format("MMM YYYY") : "Present"
}

function h(levels: number) {
  return "#".repeat(levels) + " "
}

function transformExperience(
  experience: WorkExperience
): ResumeSchema["work"][0] {
  return {
    name: experience.organization,
    location: experience.location,
    highlights: experience.highlights,
    startDate: experience.startDate,
    endDate: experience.endDate,
  }
}

function transformEducation(
  education: Education
): ResumeSchema["education"][0] {
  return {
    institution: education.name,
    studyType: education.degree,
    gpa: education.gpa.toFixed(2),
    startDate: education.startDate,
    endDate: education.endDate,
  }
}

function transformProject(project: Project): ResumeSchema["projects"][0] {
  return {
    name: project.title,
    description: project.internal.description,
    highlights: project.highlights,
    keywords: project.tags.map(tag => tag.name),
    url: "https://astrid.tech" + project.slug,
    type: "application",
  }
}

function transformResume(resume: Resume): ResumeSchema {
  return {
    basics: {
      name: "Astrid Yu",
      label: "Software Engineer",
      email: resume.email,
      phone: resume.phone,
      url: "https://astrid.tech",
      location: {
        address: resume.address,
      },
      profiles: [
        {
          network: "GitHub",
          username: "Plenglin",
          url: "https://github.com/Plenglin",
        },
        {
          network: "LinkedIn",
          username: "astrid-a-yu",
          url: "https://linkedin.com/in/astrid-a-yu",
        },
      ],
    },
    projects: resume.projects.map(transformProject),
    education: resume.schools.map(transformEducation),
    work: resume.work.map(transformExperience),
  }
}

export function generateJSONResume(resume: Resume, order: string): string {
  return JSON.stringify(transformResume(resume), null, 2)
}
