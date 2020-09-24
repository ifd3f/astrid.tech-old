import { graphql, useStaticQuery } from "gatsby"
import { FileSystemNode } from "gatsby-source-filesystem"
import React from "react"
import { Card, CardDeck, CardText, CardTitle } from "reactstrap"
import { TagList } from "src/components/tag"
import { Course, Education } from "../../types/index"
import { HomepageSection } from "./util"

function CourseCard({
  course,
  inProgress = false,
}: {
  course: Course
  inProgress?: boolean
}) {
  return (
    <Card body inverse style={{ backgroundColor: "#BD8B13" }}>
      <CardTitle style={{ marginBottom: 0, fontWeight: "bold" }}>
        {course.name}
      </CardTitle>
      <CardText style={{ marginBottom: 0 }}>{course.number}</CardText>
      <CardText style={{ marginBottom: 0 }}>{course.description}</CardText>
      <TagList tags={course.tags} link />
    </Card>
  )
}

type Query = {
  calPoly: Education
  wordmark: FileSystemNode & {
    publicURL: string
  }
  csc572: Course
  cpe357: Course
  csc348: Course
  csc400: Course
  cpe233: Course
}

export function EducationSection() {
  const result: Query = useStaticQuery(graphql`
    fragment IndexEducationSectionCourseCard on Course {
      name
      number
      slug
      description
      date
      tags {
        ...TagBadge
      }
    }
    query IndexEducationSection {
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
      wordmark: file(relativePath: { eq: "cal-poly-wordmark.svg" }) {
        publicURL
      }

      csc572: course(slug: { eq: "/education/cal-poly/csc-572/" }) {
        ...IndexEducationSectionCourseCard
      }
      cpe357: course(slug: { eq: "/education/cal-poly/cpe-357/" }) {
        ...IndexEducationSectionCourseCard
      }
      csc348: course(slug: { eq: "/education/cal-poly/csc-348/" }) {
        ...IndexEducationSectionCourseCard
      }
      csc400: course(slug: { eq: "/education/cal-poly/csc-400/" }) {
        ...IndexEducationSectionCourseCard
      }
      cpe233: course(slug: { eq: "/education/cal-poly/cpe-233/" }) {
        ...IndexEducationSectionCourseCard
      }
    }
  `)

  return (
    <HomepageSection style={{ color: "white", backgroundColor: "#154734" }}>
      <h2 className="section-heading">Education</h2>
      <div>
        <div className="float-right" style={{ paddingLeft: 15 }}>
          <img
            height="150"
            src={result.wordmark.publicURL}
            alt="Cal Poly Logo"
          />
        </div>
        <h3>California Polytechnic State University</h3>
        <p>BSc. Computer Science 2023 / GPA 3.91</p>

        <p>Click on courses for more details!</p>

        <h4>In Progress</h4>
        <CardDeck style={{ marginBottom: 20 }}>
          <CourseCard course={result.csc572} inProgress />
          <CourseCard course={result.csc400} inProgress />
        </CardDeck>

        <h4>Completed</h4>
        <CardDeck>
          <CourseCard course={result.csc348} />
          <CourseCard course={result.cpe357} />
        </CardDeck>
      </div>
    </HomepageSection>
  )
}

export default EducationSection
