package link.graph

import link.rule.{WordTag, LinkTag}
import link.tokenizer.TokenLexicon

class GraphInterpreter[W](
  sentenceTokens: Vector[W],
  lexicon: TokenLexicon[W],
) {
  def tokenTags(position: Int): Seq[WordTag] = 
    lexicon.tags(sentenceTokens(position))

  def tokenHasTag(position: Int, tag: WordTag): Boolean = 
    tokenTags(position) contains tag

  import link.graph.SentenceEdgeSyntax._
  
  def graphEdge(tag: LinkTag)(graph: ImmutableSentenceGraph.T): Option[(Int, Int)] = 
    graph.edges.map(_.outer).collectFirst {
      case x :~ y +: t if tag.matches(t) => (x, y)
    }

  def graphEdgeFrom(tag: LinkTag)(position: Int)(graph: ImmutableSentenceGraph.T): Option[Int] = 
    graph.edges.map(_.outer).collectFirst {
      case position :~ y +: t if tag.matches(t) => y
      case x :~ position +: t if tag.matches(t) => x
    }
}
