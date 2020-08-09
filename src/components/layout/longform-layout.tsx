import React, { FC, ReactNode, PropsWithChildren } from "react"
import SEO from "../seo"
import { Container, Row, Col } from "reactstrap"
import { Link } from "gatsby"
import { BsArrowLeft } from "react-icons/bs"
import style from "./longform-layout.module.scss"
import { PageHeading } from "./page-heading"

type LongformLayoutProps = {
  title: string
  description: string
  headingColor: string
  above?: ReactNode
  sidebar: ReactNode
  children: ReactNode
}

export const LongformLayout: FC<LongformLayoutProps> = ({
  title,
  description,
  headingColor,
  above,
  sidebar,
  children,
}) => {
  return (
    <>
      <SEO title={title!} description={description} />
      <PageHeading
        above={above}
        title={title}
        description={description}
        bgColor={headingColor}
      />
      <Container tag="article" className={style.container}>
        <Row>
          <Col lg={8} className={style.content}>
            {children}
          </Col>
          <Col lg={4} className={style.sidebar}>
            {sidebar}
          </Col>
        </Row>
      </Container>
    </>
  )
}

export const SidebarGroup: FC<PropsWithChildren<{}>> = ({ children }) => (
  <div className={style.sidebarGroup}>{children}</div>
)
