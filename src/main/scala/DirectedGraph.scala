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

  private def getNeighbors(vertex: V): Option[Set[V]] = edgesMap.get(vertex)

  private def traversal(
    startingVertex: V,
    nextVisitQueue: (remainingVisitors: List[V], neighbors: Set[V]) => List[V]
  ): List[V] = {

    @tailrec
    def iterate(
      visitQueue: List[V],
      visited: Set[V],
      traversalReversed: List[V]
    ): List[V] = visitQueue match {
      case Nil =>
        traversalReversed.reverse

      case currentVisitor :: remainingVisitors =>
        if (visited.contains(currentVisitor)) {
          iterate(visitQueue = remainingVisitors, visited, traversalReversed)
        } else {
          iterate(
            visitQueue = nextVisitQueue(remainingVisitors, getNeighbors(currentVisitor).getOrElse(Set.empty)),
            visited = visited + currentVisitor,
            traversalReversed = currentVisitor :: traversalReversed
          )
        }
    }

    iterate(List(startingVertex), Set.empty, List.empty)
  }

  def depthFirstTraversal(startingVertex: V): List[V] = {
    if (containsVertex(startingVertex)) {
      traversal(
        startingVertex,
        nextVisitQueue = (remainingVisitors, neighbors) => neighbors ++: remainingVisitors
      )
    } else {
      List.empty
    }
  }

  def breadthFirstTraversal(startingVertex: V): List[V] = {
    if (containsVertex(startingVertex)) {
      traversal(
        startingVertex,
        nextVisitQueue = (remainingVisitors, neighbors) => remainingVisitors ++ neighbors
      )
    } else {
      List.empty
    }
  }

}
