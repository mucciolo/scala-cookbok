package com.mucciolo

import scala.annotation.tailrec

/**
  * @tparam V vertex type
  */
final case class DirectedGraph[V](private val edgesMap: Map[V, Set[V]] = Map.empty[V, Set[V]]) {

  private type E = (V, V)

  def isEmpty: Boolean = edgesMap.isEmpty

  def withEdges(edges: E*): DirectedGraph[V] = {
    val updatedEdges = edges.foldLeft(edgesMap) { (acc, edge) =>
      val (from, to) = edge

      acc.updatedWith(from) {
          case Some(neighbors) => Some(neighbors + to)
          case None => Some(Set(to))
        }
        .updatedWith(to) {
          case Some(neighbors) => Some(neighbors)
          case None => Some(Set.empty)
        }
    }

    DirectedGraph(updatedEdges)
  }

  def containsEdge(edge: E): Boolean = {
    val (from, to) = edge
    edgesMap.get(from).exists(_.contains(to))
  }

  def containsVertex(vertex: V): Boolean = edgesMap.contains(vertex)

  val order: Int = edgesMap.size

  val size: Int = edgesMap.values.map(_.size).sum

  def depthFirstTraversal(startingVertex: V): List[V] = {
    if (containsVertex(startingVertex)) {
      depthFirstTraversalImpl(List(startingVertex), Set.empty, List.empty)
    } else {
      List.empty
    }
  }

  @tailrec
  private def depthFirstTraversalImpl(visitQueue: List[V], visited: Set[V], traversalReversed: List[V]): List[V] = {
    visitQueue match {
      case Nil => traversalReversed.reverse
      case currentVertex :: visitQueueTail =>
        if (visited.contains(currentVertex)) {
          depthFirstTraversalImpl(visitQueue = visitQueueTail, visited, traversalReversed)
        } else {
          depthFirstTraversalImpl(
            visitQueue = getNeighbors(currentVertex).getOrElse(Set.empty) ++: visitQueueTail,
            visited = visited + currentVertex,
            traversalReversed = currentVertex :: traversalReversed
          )
        }
    }
  }

  def breadthFirstTraversal(startingVertex: V): List[V] = {
    if (containsVertex(startingVertex)) {
      breadthFirstTraversalImpl(List(startingVertex), Set.empty, List.empty)
    } else {
      List.empty
    }
  }

  @tailrec
  private def breadthFirstTraversalImpl(visitQueue: List[V], visited: Set[V], traversalReversed: List[V]): List[V] = {
    visitQueue match {
      case Nil => traversalReversed.reverse
      case currentVertex :: visitQueueTail =>
        if (visited.contains(currentVertex)) {
          breadthFirstTraversalImpl(visitQueue = visitQueueTail, visited, traversalReversed)
        } else {
          breadthFirstTraversalImpl(
            visitQueue = visitQueueTail ++ getNeighbors(currentVertex).getOrElse(Set.empty),
            visited = visited + currentVertex,
            traversalReversed = currentVertex :: traversalReversed
          )
        }
    }
  }

  private def getNeighbors(vertex: V): Option[Set[V]] = edgesMap.get(vertex)

}
