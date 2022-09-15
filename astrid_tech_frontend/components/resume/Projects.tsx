import {
  Button,
  Card,
  CardImg,
  CardImgOverlay,
  CardText,
  CardTitle,
  Col,
  Container,
  Row,
} from "reactstrap";
import Link from "next/link";
import { HomepageSection } from "./util";

const ProjectCard = () => {
  return (
    <Card inverse>
      <CardImg width="300px" src={"/_/projects/astrid-tech/hero.png"} />
      <CardImgOverlay>
        <CardTitle tag="h5">Card</CardTitle>
        <CardText>Stuff</CardText>
        <Button>Do</Button>
      </CardImgOverlay>
    </Card>
  );
};

export function ProjectsSection() {
  return (
    <HomepageSection>
      <div className="">
        <h2>Interesting Projects</h2>
      </div>
      <Container fluid className="projectShowcase">
        <Row>
          <Col>
            <ProjectCard />
          </Col>
        </Row>
      </Container>
      <Link href="/projects">See more</Link>
    </HomepageSection>
  );
}
