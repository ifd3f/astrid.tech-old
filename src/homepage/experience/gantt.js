import FlatQueue from "flatqueue";

/**
 * 
 * @param {Array<Array<number>>} intervals An array of [start, end] representing the intervals. MUST BE SORTED BY start VALUES!
 * @return {Array<Array<Array<number>>>} A list of layers, which is a list of intervals. Not guaranteed to be sorted.
 */
function buildGantt(intervals) {
    if (intervals.length == 0) {
        return [];
    }
    const first = intervals[0];
    const queue = new FlatQueue();  // Priority queue of intervals, sorted by interval end ascending.
    queue.push(-first[1], [first]);  // Order ascending
    for (var i = 1; i < intervals.length; i++) {
        const interval = intervals[i];
        const [start, end] = interval;
        
        // Find the first layer we can insert into
        const supersetLayers = [];
        var createdNewLayer = false;
        while (start < -queue.peek()) {  // The earliest-ending layer's last interval ends after this interval starts
            supersetLayers.push(queue.peekValue());  
            queue.pop();
            if (queue.length == 0) {  // We can't insert into any of them!
                queue.push(-end, [interval]);  // Make a new layer
                createdNewLayer = true;
                break;
            }
        }
        if (!createdNewLayer) {  // If we haven't created a new layer
            const layer = queue.peekValue();  // This is the first layer we can successfully insert into.
            queue.pop();
            layer.push(interval);
            queue.push(-end, layer);
        }
        for (const j in supersetLayers) {  // Add in the layers we sifted through
            const supersetLayer = supersetLayers[j];
            const [, supersetLayerEnd] = supersetLayer[supersetLayer.length - 1]
            queue.push(-supersetLayerEnd, supersetLayer);
        }
    }
    return queue.values;
}

export function Gantt(props) {
    const { children } = props;
    const layers = [];
    console.log(buildGantt([
        [0, 1], [1, 3], [1, 6], [2, 4], [4, 6]
    ]));
    return "";
    
}