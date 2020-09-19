import { graphql, useStaticQuery } from "gatsby"
import React, { FC, useState } from "react"
import { BsCaretRightFill } from "react-icons/bs"
import { Course, Education } from "../../types/index"
import styleEducation from "./education.module.scss"
import { HomepageSection } from "./util"

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
  calPoly: Education
}

export function EducationSection() {
  const result: EducationQueryData = useStaticQuery(graphql`
    query GetEducationHeader {
      calPoly: school(slug: { eq: "/education/cal-poly/" }) {
        degree
        name
        startDate(formatString: "YYYY-MM")
        endDate(formatString: "YYYY-MM")
        courses {
          slug
          name
          number
          date(formatString: "YYYY-MM")
          description
        }
      }
    }
  `)

  return (
    <HomepageSection color="#80d9b7">
      <h2 className="section-heading">Education</h2>
      <EducationInfo education={result.calPoly} courseColor="#bd8b13" />
    </HomepageSection>
  )
}

export default EducationSection
