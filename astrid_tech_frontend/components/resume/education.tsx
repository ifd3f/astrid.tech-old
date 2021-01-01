import React from "react";
import { Card, CardDeck, CardText, CardTitle } from "reactstrap";
import { Course } from "../../types/types";
import { TagList } from "../tags/tag";
import { HomepageSection } from "./util";

function CourseCard({
  course,
  inProgress = false,
}: {
  course: Course;
  inProgress?: boolean;
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
  );
}

export function EducationSection() {
  return (
    <HomepageSection style={{ color: "white", backgroundColor: "#154734" }}>
      <h2 className="section-heading">Education</h2>
      {
        <div>
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
      }
    </HomepageSection>
  );
}

export default EducationSection;
