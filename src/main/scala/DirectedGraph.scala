package com.mucciolo

import scala.annotation.tailrec

/**
  * @tparam V vertex type
  */
final case class DirectedGraph[V](private val edgesMap: Map[V, Set[V]] = Map.empty[V, Set[V]]) {

  type E = (V, V)

  def isEmpty: Boolean = edgesMap.isEmpty

  def withEdges(edges: E*): DirectedGraph[V] = {
    val updatedEdges = edges.foldLeft(edgesMap) { (acc, edge) =>
      val (from, to) = edge
      acc + (from -> (acc.getOrElse(from, Set.empty) + to))
    }

    DirectedGraph(updatedEdges)
  }

  def containsEdge(edge: E): Boolean = {
    val (from, to) = edge
    edgesMap.get(from).exists(_.contains(to))
  }

  def containsVertex(vertex: V): Boolean =
    !isEmpty && (edgesMap.contains(vertex) || edgesMap.values.exists(_.contains(vertex)))

  def depthFirstTraversal(startingVertex: V): LazyList[V] = {
    if (containsVertex(startingVertex)) {
      depthFirstTraversalImpl(List(startingVertex), Set.empty, LazyList.empty)
    } else {
      LazyList.empty
    }
  }

  @tailrec
  private def depthFirstTraversalImpl(visitQueue: List[V], visited: Set[V], traversal: LazyList[V]): LazyList[V] = {
    visitQueue match {
      case Nil => traversal
      case currentVertex :: visitQueueTail =>
        if (visited.contains(currentVertex)) {
          depthFirstTraversalImpl(visitQueue = visitQueueTail, visited, traversal)
        } else {
          depthFirstTraversalImpl(
            visitQueue = getNeighbors(currentVertex).getOrElse(Set.empty) ++: visitQueueTail,
            visited = visited + currentVertex,
            traversal = traversal :+ currentVertex
          )
        }
    }
  }

  private def getNeighbors(vertex: V): Option[Set[V]] = edgesMap.get(vertex)

}
