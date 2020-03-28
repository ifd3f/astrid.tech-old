import React, { createContext, useContext, useState } from "react";
import ReactDOM from "react-dom";
import {
  Badge,
  Card,
  CardHeader,
  CardBody,
  Row,
  CardText,
  CardImg,
  Button,
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

function ExperienceInterval({ start, end, workId, title, color }) {
  const {selected, setSelected} = useContext(ExperienceContext);
  const onClick = () => setSelected(workId);
  return (
    <TimelineInterval start={start} end={end}>
      <Button active={selected === workId} onClick={onClick} style={{ width: "100%", height: "100%"}}>
        {title}
      </Button>
    </TimelineInterval>
  );
}

const ExperienceContext = createContext({
  selected: null,
});

export function ExperienceSection() {
  const [selected, setSelected] = useState("tii");

  return (
    <section>
      <ExperienceContext.Provider value={{selected, setSelected}}>
        <h2>Work experience</h2>
        <Row>
          <Col xs={2}>
          <TimelineLayer>
            <ExperienceInterval start={0} end={5} workId="foo" title="tiii" color="red"/>
            <ExperienceInterval start={1} end={4} workId="fffoo" title="tii" color="red"/>
          </TimelineLayer>
          </Col>
          <Col>
          <IronPanthersInfoCard />
          </Col>
        </Row>
      </ExperienceContext.Provider>
    </section>
  );
}
