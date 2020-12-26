import { Badge } from "reactstrap"
import { Tag, TagWrapper } from "../types/index"
import { Link, graphql } from "gatsby"
import React, { FC, ReactNode, useState } from "react"
import style from "./tag.module.scss"
import { BsCaretLeftFill } from "react-icons/bs"

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
  limit?: number
  link?: boolean
  className?: string
}

export const TagList: FC<TagListProps> = ({
  tags,
  link = false,
  limit = Number.MAX_SAFE_INTEGER,
  className,
}) => {
  const [isOpened, setOpened] = useState(false)

  const excluded = tags.slice(limit)

  const onClick = (ev: React.MouseEvent<HTMLElement, MouseEvent>) => {
    ev.preventDefault()
    ev.stopPropagation()
    setOpened(!isOpened)
  }

  const alt = isOpened
    ? "Close"
    : "Click to show: " + excluded.map(tag => tag.name).join(", ")
  const shownTags = isOpened ? tags : tags.slice(0, limit)
  const openBadgeText = isOpened ? <BsCaretLeftFill /> : <>+{excluded.length}</>

  return (
    <div className={className}>
      <p
        style={{
          fontSize: "12pt",
          marginBottom: 3,
        }}
      >
        {shownTags.map(tag => (
          <TagBadge key={tag.slug} tag={tag} link={link} />
        ))}{" "}
        {excluded.length > 0 ? (
          <Badge title={alt} onClick={onClick}>
            {openBadgeText}
          </Badge>
        ) : null}
      </p>
    </div>
  )
}
