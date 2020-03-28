import React from "react";
import {
  Badge,
  Media,
  Card,
  CardHeader,
  CardBody,
  Row,
  CardText,
  CardImg,
} from "reactstrap";
import { imgIronPanthersWide } from "../../assets";
import { Gantt } from "./gantt";

function IronPanthersInfoCard() {
  return (
    <Card style={{maxWidth: 500}}>
      <CardHeader>
        <h4>Iron Panthers Robotics Team</h4>
      </CardHeader>
      <CardImg src={imgIronPanthersWide} alt="Iron Panthers"/>
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
          <p>
            On the FIRST Robotics Competition main team, I worked to build a
            vision processing system using a Nvidia Jetson as a co-processor to
            aid the driver in aligning the robot.
          </p>
          <p>
            On the FIRST Tech Challenge subteam, I designed code for autonomous
            operation period. Created a versatile command-based system to better
            organize the auto code.
          </p>
        </CardText>
      </CardBody>
    </Card>
  );
}

export function ExperienceSection() {
  return (
    <section>
      <h2>Work experience</h2>
      <Gantt/>
        <IronPanthersInfoCard />
    </section>
  );
}
