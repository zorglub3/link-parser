package link.interpreter

import cats.data.StateT
import link.graph.ImmutableSentenceGraph
import link.graph.SentenceEdge
import link.graph.SentenceEdgeSyntax
import link.rule.WordTag
import link.rule.LinkTag

abstract class Interpreter[W] {
  type Interpret[A] = StateT[List, InterpretState, A]

  type Graph = ImmutableSentenceGraph.T

  case class InterpretState(
    words: Vector[W],
    wordTags: Vector[List[WordTag]],
    graph: Graph
  )

  def getGraph: Interpret[Graph] = StateT.get[List, InterpretState].map(_.graph)

  def collectEdge[A](f: PartialFunction[SentenceEdge, A]): Interpret[A] = {
    for {
      graph <- getGraph
      a <- StateT.liftF(graph.edges.map(_.outer).collect(f).toList)
    } yield a
  }

  import SentenceEdgeSyntax._

  def graphEdge(tag: LinkTag): Interpret[(Int, Int)] =
    collectEdge { case x :~ y +: t if tag.matches(t) => (x.min(y), x.max(y)) }

  def graphEdgeFrom(tag: LinkTag, position: Int): Interpret[Int] =
    collectEdge {
      case p :~ y +: t if tag.matches(t) && p == position => y
      case x :~ p +: t if tag.matches(t) && p == position => x
    }

  def graphEdgeLeft(tag: LinkTag, position: Int): Interpret[Int] =
    collectEdge {
      case p :~ y +: t if tag.matches(t) && p == position && y < position => y
      case x :~ p +: t if tag.matches(t) && p == position && x < position => x
    }

  def graphEdgeRight(tag: LinkTag, position: Int): Interpret[Int] =
    collectEdge {
      case p :~ y +: t if tag.matches(t) && p == position && y > position => y
      case x :~ p +: t if tag.matches(t) && p == position && x > position => x
    }
}
  
