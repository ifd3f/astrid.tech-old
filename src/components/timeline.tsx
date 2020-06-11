import FlatQueue from "flatqueue"
import React, { FC, ReactNode } from "react"
import { Row } from "reactstrap"

interface Interval {
  start: number
  end: number
}

function arrangeTimeline<T extends Interval>(intervals: T[]): T[][] {
  if (intervals.length == 0) {
    return []
  }

  const first = intervals[0]
  const queue = new FlatQueue()
  queue.push(-first.end, [first]) // Order ascending

  for (let i = 1; i < intervals.length; i++) {
    const interval = intervals[i]

    // Find the first layer we can insert into
    const supersetLayers: T[][] = []
    while (interval.start < -queue.peek() && queue.length > 0) {
      // The earliest-ending layer's last interval ends after this interval starts
      supersetLayers.push(queue.peekValue())
      queue.pop()
    }

    // We can't insert into any of them! Make a new layer.
    if (queue.length == 0) {
      queue.push(-interval.end, [interval])
    } else {
      // We can insert into the top one.
      const layer = queue.peekValue() // This is the first layer we can successfully insert into.
      queue.pop()
      layer.push(interval)
      queue.push(-interval.end, layer)
    }

    // Add back the layers we sifted through
    for (const j in supersetLayers) {
      const supersetLayer = supersetLayers[j]
      const last = supersetLayer[supersetLayer.length - 1]
      queue.push(-last.end, supersetLayer)
    }
  }

  return queue.values
}

function cleanInterval<T extends Interval>(interval: T): T {
  if (interval.start > interval.end) {
    return {
      ...interval,
      start: interval.end,
      end: interval.start,
    }
  }
  return interval
}

export interface IntervalNode extends Interval {
  content(dims: { width: number; height: number }): ReactNode
}

export interface TimelineObject {
  location: number
  content(): ReactNode
}

type TimelineProps = {
  intervals: IntervalNode[]
  objects?: TimelineObject[]
  width: number
  height: number
}

export const Timeline: FC<TimelineProps> = ({
  intervals,
  objects = [],
  width,
  height,
}) => {
  const arrangement = arrangeTimeline(intervals.map(cleanInterval))

  arrangement.sort((a, b) => a[0].end - b[0].end)

  const latest = arrangement
    .map(layer => layer[layer.length - 1].end)
    .reduce((a, b) => Math.max(a, b))

  const earliest = arrangement
    .map(layer => layer[0].start)
    .reduce((a, b) => Math.min(a, b))

  const layerWidth = width / arrangement.length
  const offsetFactor = height / (latest - earliest + 1)

  console.log(
    latest,
    earliest,
    latest - earliest,
    layerWidth,
    offsetFactor,
    arrangement
  )

  const layers = arrangement.map((layer, layerIdx) => (
    <div
      key={layerIdx}
      style={{
        height: "100%",
      }}
    >
      {layer.map(({ start, end, content }) => (
        <div
          style={{
            marginTop: offsetFactor * (start - earliest),
            marginLeft: layerWidth * layerIdx,
            width: layerWidth,
            height: offsetFactor * (start - end + 1),
            position: "absolute",
          }}
        >
          {content({
            width: layerWidth,
            height: offsetFactor * (start - end + 1),
          })}
        </div>
      ))}
    </div>
  ))

  const objectElements = objects.map(({ location, content }) => (
    <div
      style={{
        marginTop: offsetFactor * location,
        position: "absolute",
      }}
    >
      {content()}
    </div>
  ))

  return (
    <Row style={{ height: "100%", width: "100" }}>
      {layers}
      {objectElements}
    </Row>
  )
}
