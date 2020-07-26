import { Badge } from "reactstrap"
import { Tag, TagWrapper } from "../types/index"
import { Link, graphql } from "gatsby"
import React, { FC, ReactNode } from "react"
import style from "./tag.module.scss"

type TagBadgeProps = {
  tag: Tag
  link?: boolean
  children?: ReactNode
}

export const tagBadgeFragment = graphql`
  fragment TagBadge on Tag {
    name
    color
    backgroundColor
    slug
  }
`

export const TagBadge: FC<TagBadgeProps> = ({
  tag,
  link = false,
  children,
}) => {
  link = false
  const linkTo = tag.slug[0] == "/" ? tag.slug : "/tags/" + tag.slug

  return (
    <Badge
      className={style.tag}
      style={{
        backgroundColor: tag.backgroundColor,
        color: tag.color,
      }}
      tag={link ? Link : undefined}
      to={linkTo}
    >
      {tag.name}
      {children}
    </Badge>
  )
}

type TagListProps = {
  tags: Tag[]
  link?: boolean
  className?: string
}

export const TagList: FC<TagListProps> = ({
  tags,
  link = false,
  className,
}) => {
  return (
    <div className={className}>
      <p
        style={{
          fontSize: "12pt",
          marginBottom: 3,
        }}
      >
        {tags.map(tag => (
          <TagBadge key={tag.slug} tag={tag} link={link} />
        ))}
      </p>
    </div>
  )
}
