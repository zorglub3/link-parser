package link.parser

import link.rule.LinkTag
import scalax.collection.immutable.Graph
import scalax.collection.edge.LUnDiEdge

case class SentenceEdge[+N](linkTag: LinkTag, n1: N, n2: N) extends LUnDiEdge( (n1, n2) )

case class SentenceGraph[W](graph: Graph[(W, Int), LUnDiEdge]) {
  def merge(g: SentenceGraph[W]): SentenceGraph[W] = {
    val e1 = graph.edges
    val e2 = g.graph.edges

    SentenceGraph(Graph.empty ++ e1 ++ e2)
  }
}

object SentenceGraph {
  def withEdge[W](n1: (W, Int), n2: (W, Int), label: LinkTag): SentenceGraph[W] = {
    val e = SentenceEdge(label, n1, n2)
    SentenceGraph(Graph(n1, n2) + e)
  }

  def empty[W] = SentenceGraph[W](Graph())
}
