package link.graph

import scalax.collection.mutable.{Graph => MutableGraph, TypedGraphFactory, Builder}
import scalax.collection.immutable.{Graph => ImmutableGraph}

object ImmutableSentenceGraph {
  type T = ImmutableGraph[Int, SentenceEdge]

  def empty: T = from(List.empty, List.empty)

  def from(nodes: Iterable[Int], edges: Iterable[SentenceEdge]): T = 
    ImmutableGraph.from(nodes, edges)

  def newBuilder: Builder[Int, SentenceEdge, ImmutableGraph] = ImmutableGraph.newBuilder
}

object MutableSentenceGraph extends TypedGraphFactory[Int, SentenceEdge] {
  type T = MutableGraph[Int, SentenceEdge]

  def merge(g1: T, g2: T): T = ???
}

object Test {
  import SentenceEdgeSyntax._
  import link.english.lexicon.EnglishLinkTags._

  val e1 = 1 ~ 2 :+ S
  val e2 = 2 ~ 3 :+ Ss

  val g = MutableSentenceGraph.from(List(e1, e2))

  g += 3 ~ 5 :+ N
}

object Test2 {
  import SentenceEdgeSyntax._
  import link.english.lexicon.EnglishLinkTags._

  type T = ImmutableSentenceGraph.T
  val g: T = ImmutableSentenceGraph.empty

  val g2: T = g + 1
  val g3: T = g2 + (1 ~ 2 :+ S)

  val g4: T = g + 2 + 5
  val g5: T = g4 ++ List(1 ~ 2 :+ N, 2 ~ 4 :+ Ss)
  val g6: T = g4 union g3

  require(g6.contains(1 ~ 2 :+ S))

  val eOpt = g6.edges.collectFirst {
    case a :~ b +: t if a == 1 || b == 1 => (a, b, t)
  }
}
