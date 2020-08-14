import React, { FC, ReactNode, PropsWithChildren } from "react"
import SEO from "../seo"
import { Container, Row, Col } from "reactstrap"
import { Link } from "gatsby"
import { BsArrowLeft } from "react-icons/bs"
import style from "./longform-layout.module.scss"
import { PageHeading } from "./page-heading"
import { Tag } from "src/types"
import { TagList } from "../tag"

type LongformLayoutProps = {
  title: string
  description: ReactNode
  descriptionRaw: string
  headingColor: string
  above?: ReactNode
  sidebar: ReactNode
  children: ReactNode
}

export const LongformLayout: FC<LongformLayoutProps> = ({
  title,
  description,
  descriptionRaw,
  headingColor,
  above,
  sidebar,
  children,
}) => {
  return (
    <>
      <SEO title={title!} description={descriptionRaw} />
      <PageHeading
        above={above}
        title={title}
        description={description}
        bgColor={headingColor}
      />
      <Container className={style.container}>
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

type InfoRowProps = {
  name: string
  show?: any
  children: ReactNode
}

export const InfoRow: FC<InfoRowProps> = ({ name, children, show = true }) =>
  show ? (
    <tr>
      <th>{name}</th>
      <td className={style.statusData}>{children}</td>
    </tr>
  ) : null

type StatusGroupProps = {
  children: ReactNode
}

export const StatusGroup: FC<StatusGroupProps> = ({ children }) => (
  <SidebarGroup>
    <table style={{ width: "100%" }}>{children}</table>
  </SidebarGroup>
)

type TagsGroupProps = {
  tags: Tag[]
}

export const TagsGroup: FC<TagsGroupProps> = ({ tags }) => {
  return (
    <SidebarGroup>
      <h2>Tags</h2>
      <TagList tags={tags} link />
    </SidebarGroup>
  )
}
