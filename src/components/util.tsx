import React, { useState, FC, ReactNode } from "react"
import { handleViewport } from "react-in-viewport"
import { Link } from "react-router-dom"
import { Badge } from "reactstrap"
import style from "./util.module.css"

var id = 0
export function getUniqueId() {
  return id++
}

type TagBadgeProps = {
  skill: string
  link?: string
}

export const TagBadge: FC<TagBadgeProps> = ({ skill, link }) => {
  const [badgeId] = useState(`tag-badge-${getUniqueId()}`)
  return (
    <>
      <Badge
        id={badgeId}
        style={{
          backgroundColor: "#aaaaaa",
          marginRight: 2,
          marginLeft: 2,
        }}
        tag={Link}
        to={link}
      >
        {skill}
      </Badge>
    </>
  )
}

type TagListProps = {
  tags: string[]
}

export const TagList: FC<TagListProps> = ({ tags }) => {
  return (
    <div>
      <p className={style.skillsList}>
        {tags.map(tag => {
          return <TagBadge key={tag} skill={tag} />
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
