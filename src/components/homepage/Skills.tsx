import React, { Component, FC } from "react"
import handleViewport from "react-in-viewport"
import {
  Card,
  CardBody,
  CardHeader,
  Col,
  Container,
  Progress,
  Row,
} from "reactstrap"
import style from "./style.module.scss"
import { Tag } from "../../types/index"
import { useStaticQuery } from "gatsby"
import { graphql } from "gatsby"
import { TagBadge } from "../util"
import { HomepageSection } from "./util"

type AnimatedSkillBarProps = {
  level: number
}

type AnimatedSkillBarState = {
  displayed: number
}

class AnimatedSkillBarBlock extends Component<
  AnimatedSkillBarProps,
  AnimatedSkillBarState
> {
  constructor(props: AnimatedSkillBarProps) {
    super(props)
    this.state = {
      displayed: 0,
    }
  }

  render() {
    const { displayed } = this.state
    if (this.props.inViewport && this.props.level !== displayed) {
      this.setState({ displayed: this.props.level })
    }
    return <Progress value={displayed} />
  }
}

const AnimatedSkillBar: FC<AnimatedSkillBarProps> = handleViewport(
  AnimatedSkillBarBlock,
  {
    rootMargin: "-1.0px",
  }
)

type SkillInfoDisplayProps = {
  tag: Tag
  level: number
}

const SkillInfoDisplay: FC<SkillInfoDisplayProps> = ({ tag, level }) => {
  return (
    <Row>
      <Col xs={6} sm={5} md={5}>
        <TagBadge tag={tag} />
      </Col>
      <Col xs={6} sm={7} md={7}>
        <AnimatedSkillBar level={level} />
      </Col>
    </Row>
  )
}

type QueryData = {
  allSkill: {
    edges: {
      node: {
        level: number
        tag: Tag
      }
    }[]
  }
}

function SkillsSection() {
  const query: QueryData = useStaticQuery(graphql`
    query GetFeaturedSkills {
      allSkill {
        edges {
          node {
            tag {
              ...TagBadge
            }
            level
          }
        }
      }
    }
  `)

  return (
    <HomepageSection color="#223299">
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>Click on a tag to see related projects and blog posts!</p>
      </div>
      <Row>
        {query.allSkill.edges
          .map(({ node }) => node)
          .sort(({ level: a }, { level: b }) => b - a)
          .map(({ level, tag }) => (
            <Col xs={6} md={4} lg={3}>
              <SkillInfoDisplay level={level * 10} tag={tag} />
            </Col>
          ))}
      </Row>
    </HomepageSection>
  )
}

export default SkillsSection
