import React, { FC, ReactNode, useState, useEffect } from "react"
import { handleViewport } from "react-in-viewport"

var id = 0
export function getUniqueId() {
  return id++
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
