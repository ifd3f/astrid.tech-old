import { useStaticQuery, graphql } from "gatsby"
import React, { FC } from "react"
import { Education, Course } from "../../types/index"
import { HomepageSection } from "./util"
import { Container } from "reactstrap"
import { TagList } from "../util"
import styles from "./style.module.scss"

type CourseInfoProps = {
  course: Course
}

const CourseInfo: FC<CourseInfoProps> = ({ course }) => (
  <div>
    <h5>{course.name}</h5>
    <p>{course.number}</p>
    <p>{course.date}</p>
    <TagList tags={course.tags.map(({ tag }) => tag!!)} />
    {course.desc ? <p>{course.desc}</p> : null}
  </div>
)

type EducationInfoProps = {
  education: Education
}

const EducationInfo: FC<EducationInfoProps> = ({ education }) => (
  <article>
    <h4>{education.name}</h4>
    <p>{education.degree}</p>
    <p>
      {education.startDate} to {education.endDate}
    </p>
    {education.courses
      .sort(
        (a, b) => b.date.localeCompare(a.date) || a.name.localeCompare(b.name)
      )
      .map(course => (
        <CourseInfo course={course} />
      ))}
  </article>
)

type EducationQueryData = {
  allEducation: {
    edges: {
      node: Education
    }[]
  }
}

const EducationSection = () => {
  const result: EducationQueryData = useStaticQuery(graphql`
    query GetEducationHeader {
      allEducation(filter: { slug: { eq: "/education/cal-poly/" } }) {
        edges {
          node {
            degree
            name
            startDate(formatString: "YYYY-MM")
            endDate(formatString: "YYYY-MM")

            courses {
              name
              number
              date(formatString: "YYYY-MM")
              desc
              tags {
                slug
                tag {
                  name
                  slug
                  color
                  textColor
                }
              }
            }
          }
        }
      }
    }
  `)

  return (
    <HomepageSection color="#154734">
      <h2 className="section-heading">Education</h2>
      {result.allEducation.edges.map(({ node: education }) => (
        <EducationInfo education={education} />
      ))}
    </HomepageSection>
  )
}

export default EducationSection
