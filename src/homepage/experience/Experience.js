import React from "react";
import {
  Badge,
  Card,
  CardHeader,
  CardBody,
  Row,
  CardText,
  CardImg,
  Col,
} from "reactstrap";
import { imgIronPanthersWide } from "../../assets";
import { Timeline, TimelineInterval, TimelineLayer } from "./timeline";

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
      <Row>
        <Col xs={2}>
        <Timeline>
          <TimelineLayer style={{width: 50}}>
          <TimelineInterval start={0} end={3}>
            </TimelineInterval>
           <TimelineInterval start={5} end={8}>
             </TimelineInterval> 
          </TimelineLayer>
        </Timeline>
        </Col>
        <Col>
        <IronPanthersInfoCard />
        </Col>
      </Row>
    </section>
  );
}
