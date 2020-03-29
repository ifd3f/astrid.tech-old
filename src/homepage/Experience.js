import React from "react";
import {
  Badge,
  Card,
  CardBody,
  CardHeader,
  CardImg,
  CardText,
  Container,
} from "reactstrap";
import { imgIronPanthersWide } from "../assets";

function IronPanthersInfoCard() {
  return (
    <Card>
      <CardHeader>
        <h4>Iron Panthers Robotics Team</h4>
      </CardHeader>
      <CardImg src={imgIronPanthersWide} alt="Iron Panthers" />
      <CardBody>
        <CardText>
          <Badge
            pill
            color="primary"
            href="https://www.thebluealliance.com/team/5026"
          >
            FRC #5026
          </Badge>
          <Badge
            pill
            color="warning"
            href="https://theorangealliance.org/teams/7316"
          >
            FTC #7316
          </Badge>
          <Badge
            pill
            color="success"
            href="https://www.thebluealliance.com/event/2019cmptx"
          >
            FRC world champions in 2019!
          </Badge>
        </CardText>
        <CardText>
          On the FIRST Robotics Competition main team, I worked to build a
          vision processing system using a Nvidia Jetson as a co-processor to
          aid the driver in aligning the robot.
        </CardText>
        <CardText>
          On the FIRST Tech Challenge subteam, I designed code for autonomous
          operation period. Created a versatile command-based system to better
          organize the auto code.
        </CardText>
      </CardBody>
    </Card>
  );
}

export function ExperienceSection() {
  return (
    <section>
      <Container>
        <h2 className="section-header">Work Experience</h2>
        <IronPanthersInfoCard />
      </Container>
    </section>
  );
}
