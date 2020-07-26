import React, { FC, ReactNode, useState, useEffect } from "react"
import { handleViewport } from "react-in-viewport"
import crypto from "crypto"
import seedrandom from "seedrandom"
import convert from "color-convert"

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

export function hashString(str: string) {
  var hash = 0,
    chr,
    i
  for (i = 0; i < str.length; i++) {
    chr = str.charCodeAt(i)
    hash = (hash << 5) - hash + chr
    hash |= 0 // Convert to 32bit integer
  }
  return hash
}

export type PersistentColorTheme = {
  h: [number, number]
  s: [number, number]
  v: [number, number]
}

export function rescale(x: number, [a, b]: [number, number]) {
  return (b - a) * x + a
}

export const FullSpectrumTheme: PersistentColorTheme = {
  h: [0, 255],
  s: [192, 255],
  v: [192, 255],
}

export const PastelTheme: PersistentColorTheme = {
  h: [0, 360],
  s: [50, 50],
  v: [70, 70],
}

export function getPersistentColor(
  slug: string,
  theme: PersistentColorTheme = FullSpectrumTheme
): string {
  const random = seedrandom(crypto.createHash(`md5`).update(slug).digest(`hex`))
  var h = (rescale(random(), theme.h) | 0) % 360
  var s = rescale(random(), theme.s) | 0
  var v = rescale(random(), theme.v) | 0
  return `hsl(${h}, ${s}%, ${v}%)`
}
