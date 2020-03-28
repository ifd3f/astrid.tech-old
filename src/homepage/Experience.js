import React from "react";
import { Container, Media, Badge } from "reactstrap";
import { SkillsSection } from "./Skills";
import { Heading } from "./Heading";
import { imgIronPanthers } from "../assets";

function IronPanthersInfoCard() {
  return (
    <div className="card mb-3" style={{ width: 720 }}>
      <div className="row no-gutters">
        <div className="col-md-4">
          <img
            src={imgIronPanthers}
            className="card-img"
            alt="Iron Panthers Logo"
          />
        </div>
        <div className="col-md-8">
          <div className="card-body">
            <h4 className="card-title">Iron Panthers Robotics Team</h4>
            <div className="text-light">
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
            <div className="card-text">
              <p>
                On the FIRST Robotics Competition main team, I worked to build a
                vision processing system using a Nvidia Jetson as a co-processor
                to aid the driver in aligning the robot.
              </p>
              <p>
                On the FIRST Tech Challenge subteam, I designed code for
                autonomous operation period. Created a versatile command-based
                system to better organize the auto code.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

export function ExperienceSection() {
  return (
    <section>
      <h2>Work experience</h2>
      <Media>
        <IronPanthersInfoCard />
      </Media>
    </section>
  );
}
