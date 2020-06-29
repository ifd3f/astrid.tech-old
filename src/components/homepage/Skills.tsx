import React, { Component, FC, ReactNode } from "react"
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
import styleSkills from "./skills.module.scss"
import { Tag, SkillCategory } from "../../types/index"
import { useStaticQuery, Link } from "gatsby"
import { graphql } from "gatsby"
import { TagBadge } from "../util"
import { HomepageSection } from "./util"
import { BsStarFill, BsBootstrap, BsStar } from "react-icons/bs"

type StarsProps = {
  stars: number
}

const Stars: FC<StarsProps> = ({ stars }) => {
  const out: ReactNode[] = Array(5)
  var i = 0
  while (i < stars) {
    out.push(<BsStarFill />)
    i++
  }
  while (i < 5) {
    out.push(<BsStar />)
    i++
  }

  return <>{out}</>
}

type SkillInfoDisplayProps = {
  tag: Tag
  level: number
}

const SkillInfoDisplay: FC<SkillInfoDisplayProps> = ({ tag, level }) => {
  return (
    <Link to={`/tag/${tag.slug}`}>
      <div
        className={styleSkills.skillRow}
        style={{ backgroundColor: tag.color, color: tag.textColor }}
      >
        <p className={styleSkills.tagName}>{tag.name}</p>
        <p className={styleSkills.tagStars}>
          <Stars stars={level} />
        </p>
      </div>
    </Link>
  )
}

type SkillCategoryViewProps = {
  category: SkillCategory
}

const SkillCategoryView: FC<SkillCategoryViewProps> = ({
  category: { name, skills },
}) => (
  <div className={styleSkills.skillCategory}>
    <h3>{name}</h3>
    {skills
      .sort(({ level: a }, { level: b }) => b - a)
      .map(({ level, tag }) => (
        <SkillInfoDisplay level={level} tag={tag} />
      ))}
  </div>
)

type QueryData = {
  allSkill: {
    edges: {
      node: SkillCategory
    }[]
  }
}

function SkillsSection() {
  const query: QueryData = useStaticQuery(graphql`
    query GetFeaturedSkills {
      allSkill {
        edges {
          node {
            name
            skills {
              tag {
                ...TagBadge
              }
              level
            }
          }
        }
      }
    }
  `)
  console.log(query)

  return (
    <HomepageSection color="#55cdfc">
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>Click on a tag to see related projects and blog posts!</p>
      </div>
      <div className={styleSkills.skillSection}>
        {query.allSkill.edges.map(({ node }) => (
          <SkillCategoryView category={node} />
        ))}
      </div>
    </HomepageSection>
  )
}

export default SkillsSection
