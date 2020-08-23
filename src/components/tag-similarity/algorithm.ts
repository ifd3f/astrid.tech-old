/**
 * A node in a bipartite digraph without loops. This can be useful for
 * representing an object-tag system, where one part of the graph
 * is the objects and the other part is the tags, and edges represent
 * an object holding a tag.
 */
export type BipartiteNode<A, B> = {
  id: string
  neighbors: BipartiteNode<B, A>[]
  value: A
}

export function orderByResistorSimilarity<A, B>(
  neighbors: BipartiteNode<B, A>[]
) {
  const score = new Map<string, number>()
  const idToObject = new Map<string, BipartiteNode<A, B>>()

  for (let tag of neighbors) {
    const cardinality = tag.neighbors.length
    for (let other of tag.neighbors) {
      const currentScore = score.get(other.id) ?? 0
      score.set(other.id, currentScore + 1 / cardinality)
      idToObject.set(other.id, other)
    }
  }

  const objects = [...idToObject.values()]
  objects.sort((a, b) => score.get(b.id)! - score.get(a.id)!)
  return objects
}
