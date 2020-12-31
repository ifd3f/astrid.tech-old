import React, { ReactNode } from "react"
import styles from "./style.module.scss"
import { HomepageSection } from "./util"
import { Row, Col } from 'reactstrap';
import { FaHeart, FaDiceD20 } from "react-icons/fa";
import { GiNoodles } from "react-icons/gi";
import { BsClockFill } from "react-icons/bs";
import { useStaticQuery, graphql } from 'gatsby';

function BigIcon({upper, children }:{upper: ReactNode, children: ReactNode}){
  return <Col xs="12" lg="4">
    <div>{upper}</div>
    <div>{children}</div>
  </Col>
}
 
export function ElevatorSection() {
  const query: {allProject:{pageInfo:{totalCount: number}}} = useStaticQuery(graphql`
    query { 
      allProject {
        pageInfo {
          totalCount
        }
      }
    }
  `)
  return (
    <HomepageSection color="#f2f2f2">
      <Row>
        <BigIcon upper={<>9 years</>}>of experience programming</BigIcon>
        <BigIcon upper={<>{query.allProject.pageInfo.totalCount} projects</>}>tracked on this website</BigIcon>
        <BigIcon upper={<FaDiceD20/>}>Likes D&D</BigIcon>
        <BigIcon upper={<GiNoodles/>}>Enjoys cooking</BigIcon>
      </Row>
    </HomepageSection>
  )
}
