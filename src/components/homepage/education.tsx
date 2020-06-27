import { useStaticQuery, graphql } from "gatsby"
import React, { FC, useState } from "react"
import { Education, Course } from "../../types/index"
import { HomepageSection } from "./util"
import { Container } from "reactstrap"
import { TagList } from "../util"
import styles from "./style.module.scss"
import styleEducation from "./education.module.scss"
import { BsCaretRight, BsCaretRightFill } from "react-icons/bs"

type CourseInfoProps = {
  course: Course
  color: string
  selected?: boolean
  onMouseEnter?: (course: Course) => void
  onMouseLeave?: (course: Course) => void
}

const CourseInfo: FC<CourseInfoProps> = ({
  course,
  color,
  selected = false,
  onMouseEnter: _onMouseEnter = () => {},
  onMouseLeave: _onMouseLeave = () => {},
}) => {
  const onMouseEnter = () => {
    _onMouseEnter(course)
  }
  const onMouseLeave = () => {
    _onMouseLeave(course)
  }

  return (
    <div
      className={styleEducation.courseInfo}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
    >
      <div
        className={styleEducation.mainBlock}
        style={{ backgroundColor: color }}
      >
        <div className={styleEducation.content}>
          <h5>{course.number}</h5>
          <p className={styleEducation.courseTitle}>{course.name}</p>
        </div>
        <div>
          <div className={styleEducation.arrowWrapper}>
            <BsCaretRightFill className={styleEducation.arrow} />
          </div>
          <div
            className={styleEducation.selectionLineWrapper}
            style={{ visibility: selected ? "visible" : "collapse" }}
          >
            <div
              className={styleEducation.selectionLine}
              style={{ backgroundColor: color }}
            ></div>
          </div>
        </div>
      </div>
    </div>
  )
}

type CourseDetailProps = {
  course: Course
}

const CourseDetail: FC<CourseDetailProps> = ({ course }) => {
  return <div className={styleEducation.courseDetail}>{course.slug}</div>
}

type CourseDataDisplayProps = {
  courses: Course[]
  color: string
}

const CourseDataDisplay: FC<CourseDataDisplayProps> = ({ courses, color }) => {
  const [displayedCourse, setDisplayedCourse] = useState(courses[0])

  const onMouseEnter = (course: Course) => {
    setDisplayedCourse(course)
  }

  return (
    <div>
      <div>
        {courses
          .sort(
            (a, b) =>
              b.date.localeCompare(a.date) || a.name.localeCompare(b.name)
          )
          .map(course => (
            <CourseInfo
              selected={course.slug == displayedCourse.slug}
              course={course}
              color={color}
              onMouseEnter={onMouseEnter}
            />
          ))}
      </div>
      <CourseDetail course={displayedCourse} />
    </div>
  )
}

type EducationInfoProps = {
  education: Education
  courseColor: string
}

const EducationInfo: FC<EducationInfoProps> = ({ education, courseColor }) => (
  <article className={styleEducation.educationArticle}>
    <div className={styleEducation.articleHeading}>
      <h3>{education.name}</h3>
      <div className={styleEducation.subheader}>
        <p className={styleEducation.degree}>{education.degree}</p>
        <p className={styleEducation.duration}>
          {education.startDate} to {education.endDate}
        </p>
      </div>
    </div>
    <h4>Classes Taken</h4>
    <CourseDataDisplay courses={education.courses} color={courseColor} />
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
              slug
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
      <div>
        {result.allEducation.edges.map(({ node: education }) => (
          <EducationInfo education={education} courseColor="#bd8b13" />
        ))}
      </div>
    </HomepageSection>
  )
}

export default EducationSection
