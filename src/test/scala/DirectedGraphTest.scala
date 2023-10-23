package com.mucciolo

import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

final class DirectedGraphTest extends AnyWordSpec with should.Matchers with TableDrivenPropertyChecks {
  "A directed graph" when {

    "created" should {
      val graph = DirectedGraph[Int]()

      "be empty" in {
        graph.isEmpty shouldBe true
      }

      "return empty list on DFT" in {
        graph.depthFirstTraversal(1) shouldBe List.empty
      }
    }

    "not containing vertex 3" should {
      val graph = DirectedGraph[Int]().withEdges(1 -> 2)

      "return false" in {
        graph.containsVertex(3) shouldBe false
      }

      "return true after incident with an edge" in {
        Table(
          "edge",
          2 -> 3,
          3 -> 2,
          3 -> 3,
          1 -> 3,
          4 -> 3,
          3 -> 4
        ).forEvery { edge =>
          graph.withEdges(edge).containsVertex(3) shouldBe true
        }
      }
    }

    "not containing edge 2 -> 3" should {
      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 1 -> 3, 3 -> 2, 3 -> 1)

      "return false" in {
        graph.containsEdge(2 -> 3) shouldBe false
      }

      "return true after edge is added" in {
        graph.withEdges(2 -> 3).containsEdge(2 -> 3) shouldBe true
      }
    }

    "composed of two linked different vertexes" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2)

      "not be empty" in {
        graph.isEmpty shouldBe false
      }

      "contain vertex 1" in {
        graph.containsVertex(1) shouldBe true
      }

      "contain vertex 2" in {
        graph.containsVertex(2) shouldBe true
      }

      "contain edge 1 -> 2" in {
        graph.containsEdge(1 -> 2) shouldBe true
      }

      "not contain the edge 1 -> 2 reverse" in {
        graph.containsEdge(2 -> 1) shouldBe false
      }

      "handle DFT starting at root 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2)
      }

      "handle DFT starting at leaf 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2)
      }

    }

    "composed of two linked equal vertexes" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 1)

      "contain vertex 1" in {
        graph.containsVertex(1) shouldBe true
      }

      "contain edge 1 -> 1" in {
        graph.containsEdge(1 -> 1) shouldBe true
      }

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1)
      }

    }

    "path graph of order 2" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3)

      "handle DFT starting at root 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3)
      }

      "handle DFT starting at middle vertex 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3)
      }

      "handle DFT starting at leaf 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3)
      }

    }

    "tree with two branches of depth 2" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 4, 1 -> 3, 3 -> 5)      

      "handle DFT starting at root 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 4, 3, 5)
      }

      "handle DFT starting at middle 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 4)
      }

      "handle DFT starting at leaf 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4)
      }

      "handle DFT starting at middle 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 5)
      }

      "handle DFT starting at leaf 5" in {
        graph.depthFirstTraversal(5) shouldBe List(5)
      }

    }

    "tree with subtree" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3, 2 -> 4)

      "handle DFT starting at root 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3, 4)
      }

      "handle DFT starting at middle 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3, 4)
      }

      "handle DFT starting at leaf 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3)
      }

      "handle DFT starting at leaf 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4)
      }

    }

    "tree with three branches" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 1 -> 3, 1 -> 4)

      "handle DFT starting at root 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3, 4)
      }

      "handle DFT starting at middle 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2)
      }

      "handle DFT starting at middle 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3)
      }

      "handle DFT starting at middle 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4)
      }

    }

    "3-cyclic" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3, 3 -> 1)

      "handle DFT starting at root 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3)
      }

      "handle DFT starting at middle vertex 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3, 1)
      }

      "handle DFT starting at leaf 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 1, 2)
      }

    }

    "3-cyclic attached with two trees" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3, 3 -> 1, 1 -> 4, 1 -> 5)

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3, 4, 5)
      }

      "handle DFT starting at 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3, 1, 4, 5)
      }

      "handle DFT starting at 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 1, 2, 4, 5)
      }

      "handle DFT starting at 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4)
      }

      "handle DFT starting at 5" in {
        graph.depthFirstTraversal(5) shouldBe List(5)
      }

    }

    "disconnected" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 3 -> 4)

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2)
      }

      "handle DFT starting at 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2)
      }

      "handle DFT starting at 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 4)
      }

      "handle DFT starting at 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4)
      }

    }

    "disconnected with 3-cyclic" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3, 3 -> 1, 4 -> 5)

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3)
      }

      "handle DFT starting at 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3, 1)
      }

      "handle DFT starting at 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 1, 2)
      }

      "handle DFT starting at 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4, 5)
      }

      "handle DFT starting at 5" in {
        graph.depthFirstTraversal(5) shouldBe List(5)
      }

    }

    "composed of two connected 3-cycles" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3, 3 -> 1, 4 -> 5, 5 -> 6, 6 -> 4, 1 -> 4)

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3, 4, 5, 6)
      }

      "handle DFT starting at 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3, 1, 4, 5, 6)
      }

      "handle DFT starting at 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 1, 2, 4, 5, 6)
      }

      "handle DFT starting at 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4, 5, 6)
      }

      "handle DFT starting at 5" in {
        graph.depthFirstTraversal(5) shouldBe List(5, 6, 4)
      }

      "handle DFT starting at 6" in {
        graph.depthFirstTraversal(6) shouldBe List(6, 4, 5)
      }

    }

    "is a tree of degree 2 and height 2" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 1 -> 3, 2 -> 4, 2 -> 5, 3 -> 6, 3 -> 7)

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 4, 5, 3, 6, 7)
      }

      "handle DFT starting at 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 4, 5)
      }

      "handle DFT starting at 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 6, 7)
      }

      "handle DFT starting at 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4)
      }

      "handle DFT starting at 5" in {
        graph.depthFirstTraversal(5) shouldBe List(5)
      }

      "handle DFT starting at 6" in {
        graph.depthFirstTraversal(6) shouldBe List(6)
      }

      "handle DFT starting at 7" in {
        graph.depthFirstTraversal(7) shouldBe List(7)
      }

    }

    "two 1-intersecting 3-cycles" should {

      val graph = DirectedGraph[Int]().withEdges(1 -> 2, 2 -> 3, 3 -> 1, 1 -> 4, 4 -> 5, 5 -> 3)

      "handle DFT starting at 1" in {
        graph.depthFirstTraversal(1) shouldBe List(1, 2, 3, 4, 5)
      }

      "handle DFT starting at 2" in {
        graph.depthFirstTraversal(2) shouldBe List(2, 3, 1, 4, 5)
      }

      "handle DFT starting at 3" in {
        graph.depthFirstTraversal(3) shouldBe List(3, 1, 2, 4, 5)
      }

      "handle DFT starting at 4" in {
        graph.depthFirstTraversal(4) shouldBe List(4, 5, 3, 1, 2)
      }

      "handle DFT starting at 5" in {
        graph.depthFirstTraversal(5) shouldBe List(5, 3, 1, 2, 4)
      }
    }

    "undirected 3-complete graph" should {

        val graph = DirectedGraph[Int]().withEdges(1 -> 2, 1 -> 3, 2 -> 1, 2 -> 3, 3 -> 1, 3 -> 2)

        "handle DFT starting at 1" in {
          graph.depthFirstTraversal(1) shouldBe List(1, 2, 3)
        }

        "handle DFT starting at 2" in {
          graph.depthFirstTraversal(2) shouldBe List(2, 1, 3)
        }

        "handle DFT starting at 3" in {
          graph.depthFirstTraversal(3) shouldBe List(3, 1, 2)
        }

    }

  }
}
