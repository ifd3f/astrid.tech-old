import { graphql, Link, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { BsStar, BsStarFill } from "react-icons/bs"
import Masonry from "react-masonry-component"
import { Col } from "reactstrap"
import { SkillGroup, Tag } from "../../types/index"
import styleSkills from "./skills.module.scss"
import style from "./style.module.scss"
import { HomepageSection } from "./util"

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
    <Link to={`/tags/${tag.slug}`}>
      <div
        className={styleSkills.skillRow}
        style={{ backgroundColor: tag.backgroundColor, color: tag.color }}
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
  category: SkillGroup
}

const SkillCategoryView: FC<SkillCategoryViewProps> = ({
  category: { name, skills },
}) => (
  <Col xs="12" sm="6" lg="4">
    <h3>{name}</h3>
    {skills
      .sort(({ level: a }, { level: b }) => b - a)
      .map(({ level, tag }) => (
        <SkillInfoDisplay level={level} tag={tag} />
      ))}
  </Col>
)

type QueryData = {
  allSkillGroup: {
    nodes: SkillGroup[]
  }
}

export function SkillsSection() {
  const query: QueryData = useStaticQuery(graphql`
    query GetFeaturedSkills {
      allSkillGroup {
        nodes {
          id
          name
          skills {
            level
            tag {
              color
              name
              slug
              backgroundColor
            }
          }
        }
      }
    }
  `)

  return (
    <HomepageSection color="#55cdfc">
      <div className={style.sectionHeading}>
        <h2>Skills</h2>
        <p>Click on a tag to see related projects and blog posts!</p>
      </div>
      <Masonry options={{ transitionDuration: 0 }}>
        {query.allSkillGroup.nodes.map(node => (
          <SkillCategoryView category={node} />
        ))}
      </Masonry>
    </HomepageSection>
  )
}
