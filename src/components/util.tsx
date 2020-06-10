import { Link } from "gatsby"
import React, { FC, ReactNode, useState } from "react"
import { handleViewport } from "react-in-viewport"
import { Badge } from "reactstrap"
import { Tag } from "../types/index"

var id = 0
export function getUniqueId() {
  return id++
}

type TagBadgeProps = {
  tag: Tag
}

export const TagBadge: FC<TagBadgeProps> = ({ tag }) => {
  const [badgeId] = useState(`tag-badge-${getUniqueId()}`)
  return (
    <>
      <Badge
        id={badgeId}
        style={{
          backgroundColor: tag.color,
          color: tag.textColor,
          marginRight: 2,
          marginLeft: 2,
        }}
        tag={Link}
        to={"/tag/" + tag.slug}
      >
        {tag.name}
      </Badge>
    </>
  )
}

type TagListProps = {
  tags: Tag[]
}

export const TagList: FC<TagListProps> = ({ tags }) => {
  return (
    <div>
      <p
        style={{
          fontSize: "12pt",
        }}
      >
        {tags.map(tag => {
          return <TagBadge tag={tag} />
        })}
      </p>
    </div>
  )
}

type LoadOnViewProps = {
  inViewport: boolean
  forwardedRef: any
  children: ReactNode
}

const LoadOnViewBlock: FC<LoadOnViewProps> = ({
  inViewport,
  forwardedRef,
  children,
}) => {
  const [shown, setShown] = useState(inViewport)
  if (!shown && inViewport) {
    setShown(true)
  }
  return <div ref={forwardedRef}>{shown ? children : null}</div>
}

export const LoadOnView = handleViewport(LoadOnViewBlock)
