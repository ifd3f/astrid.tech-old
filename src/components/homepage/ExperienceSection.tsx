import { graphql, useStaticQuery } from "gatsby"
import React, { FC, ReactNode } from "react"
import { Container, Badge } from "reactstrap"
import { WorkExperience } from "../../types/index"
import style from "./style.module.scss"

const IronPanthersTagline = () => {
  return (
    <p style={{ display: "inline-block", marginBottom: 5, marginRight: 5 }}>
      <Badge
        pill={true}
        color="primary"
        href="https://www.thebluealliance.com/team/5026"
      >
        FRC #5026
      </Badge>{" "}
      <Badge
        pill={true}
        color="warning"
        href="https://theorangealliance.org/teams/7316"
      >
        FTC #7316
      </Badge>
      <a href="https://theburlingameb.org/1990/news/iron-panthers-win-world-championships/">
        World
      </a>{" "}
      <a href="https://en.wikipedia.org/wiki/Burlingame_High_School_(California)#Robotics">
        Champion
      </a>
      <a href="https://www.businesswire.com/news/home/20190420005006/en/Youth-Robotics-Teams-Inspire-Record-Crowds-FIRST%C2%AE">
        ship
      </a>
      -
      <a href="https://www.smdailyjournal.com/news/local/burlingame-high-school-claims-robotics-crown/article_8a3bb226-6895-11e9-9d1a-9b53ee5976f3.html">
        Winning
      </a>{" "}
      Robotics Team
    </p>
  )
}
type ArticleProps = {
  experience: WorkExperience
  tagline?: ReactNode
}

const Article: FC<ArticleProps> = ({ experience, tagline: _tagline }) => {
  let tagline: ReactNode
  if (_tagline != null) {
    tagline = _tagline
  } else if (experience.summary) {
    tagline = <p>{experience.summary}</p>
  } else {
    tagline = null
  }

  const durationText =
    experience.endDate == null
      ? experience.startDate
      : `${experience.startDate} to ${experience.endDate}`

  return (
    <article>
      <div>
        <div className={style.mainHeader}>
          <h4 className={style.positionTitle}>
            {experience.position} at{" "}
            <a href={experience.website}>{experience.organization}</a>
          </h4>
          <div className={style.durationTitleWrapper}>
            <p className={style.durationTitle}>{durationText}</p>
          </div>
        </div>
        {tagline}
      </div>

      <div>
        <ul>
          {experience.highlights.map(h => (
            <li>{h}</li>
          ))}
        </ul>
      </div>
    </article>
  )
}

type QueryData = {
  allWorkExperienceYaml: {
    edges: [
      {
        node: WorkExperience
      },
      {
        node: WorkExperience
      },
      {
        node: WorkExperience
      }
    ]
  }
}

export const ExperienceSection = () => {
  const query: QueryData = useStaticQuery(graphql`
    {
      allWorkExperienceYaml(
        filter: { slug: { in: ["fabtime", "iron-panthers", "micro-vu"] } }
        sort: { fields: [slug], order: ASC }
      ) {
        edges {
          node {
            startDate(formatString: "YYYY-MM")
            endDate(formatString: "YYYY-MM")
            highlights
            id
            location
            organization
            position
            tags
            summary
            type
            website
            slug
          }
        }
      }
    }
  `)

  const [
    { node: fabtime },
    { node: ironPanthers },
    { node: microvu },
  ] = query.allWorkExperienceYaml.edges

  return (
    <Container className={style.experienceSection} tag="section">
      <h2 className={style.sectionHeading}>Work Experience</h2>
      <Article experience={microvu} />
      <Article experience={fabtime} />
      <Article experience={ironPanthers}>
        <IronPanthersTagline />
      </Article>
    </Container>
  )
}

export default ExperienceSection
