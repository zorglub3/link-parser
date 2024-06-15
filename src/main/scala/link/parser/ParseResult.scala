package link.parser

import link.graph.SentenceEdgeSyntax
import link.graph.ImmutableSentenceGraph
import link.graph.ImmutableSentenceGraph.{T => SentenceGraph}
import link.rule.{WordTag, LinkTag}

case class ParseResult[W](
  graph: SentenceGraph,
  words: Vector[W],
  tags: Vector[List[WordTag]]
) {
  def addLink(w1: Int, w2: Int, linkTag: LinkTag): ParseResult[W] = {
    import SentenceEdgeSyntax._
    val link = w1 ~ w2 :+ linkTag.simplify
    
    ParseResult(
      graph + link,
      words,
      tags,
    )
  }

  def tagWord(index: Int, newTags: List[WordTag]): ParseResult[W] = {
    ParseResult(
      graph,
      words,
      Vector.tabulate(words.length) { n =>
        if(n == index) {
          (newTags ++ tags(n)).distinct
        } else {
          tags(n)
        }
      }
    )
  }

  def merge(r2: ParseResult[W]): ParseResult[W] = {
    require(r2.words == words)

    val mergedTags = Vector.tabulate(words.length) { n =>
      (tags(n) ++ r2.tags(n)).distinct
    }

    ParseResult(
      graph union r2.graph,
      words,
      mergedTags,
    )  
  }

  def tokenTags(position: Int): Seq[WordTag] =
    tags.lift(position).getOrElse(Seq.empty)

  def tokenHasTag(position: Int, tag: WordTag): Boolean = 
    tokenTags(position).contains(tag)

  import link.graph.SentenceEdgeSyntax._

  def graphEdge(tag: LinkTag): Option[(Int, Int)] =
    graph.edges.map(_.outer).collectFirst {
      case x :~ y +: t if tag.matches(t) => (x min y, x max y)
    }    

  def graphEdgeFrom(tag: LinkTag)(position: Int): Option[Int] =
    graph.edges.map(_.outer).collectFirst {
      case p :~ y +: t if tag.matches(t) && p == position => y
      case x :~ p +: t if tag.matches(t) && p == position => x
    }

  def allEdgesFrom(tag: LinkTag)(position: Int): List[Int] = 
    graph.edges.map(_.outer).collect {
      case p :~ y +: t if tag.matches(t) && p == position => y
      case x :~ p +: t if tag.matches(t) && p == position => x
    } .toList

  def graphEdgeLeft(tag: LinkTag)(position: Int): Option[Int] =
    graph.edges.map(_.outer).collectFirst {
      case x :~ p +: t if tag.matches(t) && p == position && x < p => x
    }

  def graphEdgeRight(tag: LinkTag)(position: Int): Option[Int] =
    graph.edges.map(_.outer).collectFirst {
      case p :~ x +: t if tag.matches(t) && p == position && x > p => x
    }
}

object ParseResult {
  def withLink[W](words: Vector[W], w1: Int, w2: Int, linkTag: LinkTag): ParseResult[W] = {
    import SentenceEdgeSyntax._
    import scala.language.postfixOps

    ParseResult(
      ImmutableSentenceGraph.from(
        0 until words.length toList,
        List(w1 ~ w2 :+ linkTag.simplify),
      ),
      words,
      Vector.fill(words.length)(List.empty),
    )
  }

  def emptyFromWords[W](words: Vector[W]): ParseResult[W] = {
    import scala.language.postfixOps

    ParseResult(
      ImmutableSentenceGraph.from(0 until words.length toList, List.empty),
      words,
      Vector.fill(words.length)(List.empty),
    )
  }

  def withTags[W](words: Vector[W], index: Int, tags: List[WordTag]): ParseResult[W] = {
    import scala.language.postfixOps

    ParseResult(
      ImmutableSentenceGraph.from(0 until words.length toList, List.empty),
      words,
      Vector.tabulate(words.length) { n =>
        if(n == index) {
          tags
        } else {
          List.empty
        }
      }
    )
  }
}
