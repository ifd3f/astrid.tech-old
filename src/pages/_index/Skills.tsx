import { graphql, Link, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { BsStar, BsStarFill } from "react-icons/bs"
import { SkillCategory, Tag } from "../../types/index"
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
    <Link to={`/tag/${tag.slug}`}>
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
  allSkillGroup: {
    nodes: SkillCategory[]
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
      <div className={styleSkills.skillSection}>
        {query.allSkillGroup.nodes.map(node => (
          <SkillCategoryView category={node} />
        ))}
      </div>
    </HomepageSection>
  )
}
