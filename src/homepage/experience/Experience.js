import React, { createContext, useContext, useState } from "react";
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
  Carousel,
  CarouselItem,
  CarouselIndicators,
  CarouselControl,
  CarouselCaption
} from "reactstrap";
import { imgIronPanthersWide } from "../../assets";
import { TimelineInterval, TimelineLayer } from "./timeline";

function IronPanthersInfoCard() {
  return (
    <Card>
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

const ExperienceContext = createContext(null);

function dateIndex(year, month) {
  return year * 12 + month;
}

const WORK_EXPERIENCES = [
  {
    id: "ironPanthers",
    title: "Iron Panthers Robotics Team",
    info: <IronPanthersInfoCard/>,
    start: dateIndex(2014, 8),
    end: dateIndex(2019, 5)
  },
  {
    id: "goodRipple",
    title: "GoodRipple Inc.",
    info: <div><p>WIP</p></div>,
    start: dateIndex(2018, 6),
    end: dateIndex(2018, 7),
  },
  {
    id: "fabTime",
    title: "FabTime Inc.",
    info: <div><p>WIP</p></div>,
    start: dateIndex(2019, 11),
    end: dateIndex(2020, 6)
  }
]

function ExperienceCardCarousel({ items }) {
  const [activeIndex, setActiveIndex] = useState(0);
  const [animating, setAnimating] = useState(false);

  const next = () => {
    if (animating) return;
    const nextIndex = activeIndex === items.length - 1 ? 0 : activeIndex + 1;
    setActiveIndex(nextIndex);
  }

  const previous = () => {
    if (animating) return;
    const nextIndex = activeIndex === 0 ? items.length - 1 : activeIndex - 1;
    setActiveIndex(nextIndex);
  }

  const goToIndex = (newIndex) => {
    if (animating) return;
    setActiveIndex(newIndex);
  }

  const slides = items.map((item) => {
    return (
      <CarouselItem
        onExiting={() => setAnimating(true)}
        onExited={() => setAnimating(false)}
        key={item.id}
      >
        {item.info}
        <CarouselCaption captionText={item.caption} captionHeader={item.caption} />
      </CarouselItem>
    );
  });

  return (
    <Carousel
      activeIndex={activeIndex}
      next={next}
      previous={previous}
    >
      <CarouselIndicators items={items} activeIndex={activeIndex} onClickHandler={goToIndex} />
      {slides}
      <CarouselControl direction="prev" directionText="Previous" onClickHandler={previous} />
      <CarouselControl direction="next" directionText="Next" onClickHandler={next} />
    </Carousel>
  );
}

function ExperienceTimelineCarousel({items}) {
  const [selected, setSelected] = useState("ironPanthers");
  const earliestIndex = Math.min.apply(null, items.map(e => e.start));

  return (
    <ExperienceContext.Provider value={{selected, setSelected}}>
      <h2>Work experience</h2>
      <Row>
        <Col xs={2}>
          <TimelineLayer>
            {
              items.map(e => (
                <ExperienceInterval start={e.start - earliestIndex} end={e.end - earliestIndex} workId={e.id} title={e.title} color={e.color}/>
              ))
            }
          </TimelineLayer>
        </Col>
        <Col>
          <ExperienceCardCarousel items={items}/>
        </Col>
      </Row>
    </ExperienceContext.Provider>
  )
}

export function ExperienceSection() {

  return (
    <section>
      <ExperienceTimelineCarousel items={WORK_EXPERIENCES}/>
    </section>
  );
}
