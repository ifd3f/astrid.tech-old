import React from "react";
import { Badge, Card, CardBody, CardHeader, CardImg, CardLink, CardText, Container } from "reactstrap";
import { imgIronPanthersWide } from "../assets";

function WorkExperienceCard({ children }) {
  return <Card style={{ margin: 20 }}>{children}</Card>;
}

function IronPanthersInfoCard() {
  return (
    <WorkExperienceCard>
      <CardHeader>
        <h4>Programmer on Iron Panthers Robotics Team</h4>
      </CardHeader>
      <CardImg src={imgIronPanthersWide} alt="Iron Panthers" />
      <CardBody>
        <div className="d-flex">
          <div>
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
          </div>
          <div className="ml-auto">
            <CardLink
              pill
              color="info"
              target="_blank"
              href="https://ironpanthers.com"
            >
              Website
            </CardLink>
          </div>
        </div>
        <CardText>
          On the FIRST Robotics Competition main team, I worked to build a
          vision processing system using a Nvidia Jetson as a co-processor to
          aid the driver in aligning the robot. This was achieved through a
          combination of OpenCV and a neural network.
        </CardText>
        <CardText>
          On the FIRST Tech Challenge subteam, I designed code to score points
          during the autonomous operation period. I created a versatile
          command-based system based on FRC's system to make the auto code more
          organized and logical, eliminating issues.
        </CardText>
      </CardBody>
    </WorkExperienceCard>
  );
}

function FabTimeInfoCard() {
  return (
    <WorkExperienceCard>
      <CardHeader>
        <h4>Software Intern at FabTime Inc.</h4>
      </CardHeader>
      <CardBody>

      </CardBody>
    </WorkExperienceCard>
  );
}

function GoodRippleInfoCard() {
  return (
    <WorkExperienceCard>
      <CardHeader>
        <h4>Software Intern at GoodRipple Inc.</h4>
      </CardHeader>
      <CardBody>
        <CardText>foo</CardText>
      </CardBody>
    </WorkExperienceCard>
  );
}

export function ExperienceSection() {
  return (
    <section>
      <Container>
        <h2 className="section-header">Work Experience</h2>
        <FabTimeInfoCard/>
        <GoodRippleInfoCard />
        <IronPanthersInfoCard />
      </Container>
    </section>
  );
}
