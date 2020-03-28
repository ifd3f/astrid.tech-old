import React, { useContext, createContext } from "react";
import { Row, Col } from "reactstrap";
import FlatQueue from "flatqueue";

/**
 * 
 * @param {Array<Array<number>>} intervals An array of objects with {start, end, value} representing the intervals. MUST BE SORTED BY start VALUES!
 * @return {Array<Array<Array<number>>>} A list of layers, which is a list of intervals. Not guaranteed to be sorted.
 */
function buildGantt(intervals) {
    if (intervals.length == 0) {
        return [];
    }
    const first = intervals[0];
    const queue = new FlatQueue();  // Priority queue of intervals, sorted by interval end ascending.
    queue.push(-first.end, [first]);  // Order ascending
    for (var i = 1; i < intervals.length; i++) {
        const interval = intervals[i];
        
        // Find the first layer we can insert into
        const supersetLayers = [];
        var createdNewLayer = false;
        while (interval.start < -queue.peek()) {  // The earliest-ending layer's last interval ends after this interval starts
            supersetLayers.push(queue.peekValue());  
            queue.pop();
            if (queue.length == 0) {  // We can't insert into any of them!
                queue.push(-interval.end, [interval]);  // Make a new layer
                createdNewLayer = true;
                break;
            }
        }
        if (!createdNewLayer) {  // If we haven't created a new layer
            const layer = queue.peekValue();  // This is the first layer we can successfully insert into.
            queue.pop();
            layer.push(interval);
            queue.push(-interval.end, layer);
        }
        for (const j in supersetLayers) {  // Add in the layers we sifted through
            const supersetLayer = supersetLayers[j];
            const last = supersetLayer[supersetLayer.length - 1]
            queue.push(-last.end, supersetLayer);
        }
    }
    return queue.values;
}

export const INTERVAL = 0;
export const INSTANT = 1;

export function TimelineInterval({ offsetFactor = 10, start, end, children }) {
    const height = offsetFactor * (end - start);

    return (
        <div style={{ position: "absolute", width: "100%", height: height, marginTop: offsetFactor * start }}>
            {children}
        </div>
    )
}

export function TimelineLayer({ children, width = 30 }) {
   
    const intervals = children.map(child => {
        const { start, end } = child.props;
        return {
            start, end, value: child
        };
    });
    const gantt = buildGantt(intervals);

    const layers = gantt.map((layer, i) => (
        <Col key={i}>
            {
                layer.map(x => x.value)
            }
        </Col>
    ));

    return (
        <Row>
            {layers}
        </Row>
    )
}

const TimelineContext = createContext({ offsetFactor: 10 });

export function Timeline({ children }) {

    return (
        <TimelineContext.Provider>
            <Row>
                {children}
            </Row>
        </TimelineContext.Provider>
    );
    
}