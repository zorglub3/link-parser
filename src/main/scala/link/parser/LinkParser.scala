package link.parser

import cats.implicits._
import link.rule._
import link.graph.ImmutableSentenceGraph
import link.graph.SentenceEdgeSyntax
import link.graph.ImmutableSentenceGraph.{T => SentenceGraph}

class LinkParser[W](val ruleMap: Map[W, LinkRule.NormalForm]) {
  def sentenceRules(sentence: List[W]): Option[List[LinkRule.NormalForm]] = 
    sentence.traverse(ruleMap.get _)

  def check(words: List[W]): Int = {
    sentenceRules(words).fold(0) { rules =>
      def linkMatch(a: List[LinkRule.RightLink], b: List[LinkRule.LeftLink]): Boolean = {
        (a, b) match {
          case (ha :: _, hb :: _) => ha.linkTag.matches(hb.linkTag)
          case _ => false
        }
      }

      def count(leftIndex: Int, rightIndex: Int, l: List[LinkRule.LeftLink], r: List[LinkRule.RightLink]): Int = {
        if(leftIndex + 1 == rightIndex) {
          if(l.isEmpty && r.isEmpty) {
            1
          } else {
            0
          }
        } else {
          var total = 0

          for(w <- leftIndex + 1 until rightIndex) {
            for(d <- rules(w).disjunction) {
              val leftCount = if(linkMatch(r, d.leftLinks)) {
                count(leftIndex, w, d.leftLinks.tail, r.tail)
              } else {
                0
              }

              val rightCount = if(linkMatch(d.rightLinks, l)) {
                count(w, rightIndex, l.tail, d.rightLinks.tail)
              } else {
                0
              }

              total += leftCount * rightCount

              if(leftCount > 0) {
                total += leftCount * count(w, rightIndex, l, d.rightLinks)
              }

              if(rightCount > 0) {
                total += rightCount * count(leftIndex, w, d.leftLinks, r)
              }
            }
          }

          total
        }
      }

      def conjunctParses(start: Int, conjunct: LinkRule.LinkList): Int = {
        if(conjunct.leftLinks.isEmpty) {
          if(conjunct.rightLinks.isEmpty && start < words.length) {
            rules(start + 1).disjunction.map { conjunct2 =>
              conjunctParses(start + 1, conjunct2)
            } .sum
          } else {
            count(start, words.length, List.empty, conjunct.rightLinks)
          }
        } else {
          0
        }
      }

      rules(0).disjunction.map { conjunct =>
        conjunctParses(0, conjunct)
      } .sum
    }
  }

  def links(words: List[W]): List[SentenceGraph] = {
    import SentenceEdgeSyntax._

    sentenceRules(words).fold(List.empty[SentenceGraph]) { rules =>
      def makeLink(w1: Int, w2: Int, as: List[LinkRule.RightLink], bs: List[LinkRule.LeftLink]): List[SentenceGraph] = 
        (as, bs) match {
          case (a :: _, b :: _) if a.linkTag.matches(b.linkTag) => {
            List(ImmutableSentenceGraph.from(List(w1, w2), List(w1 ~ w2 :+ a.linkTag.simplify)))
          }
          case _ => List.empty
        }

      def product(as: List[SentenceGraph], bs: => List[SentenceGraph]): List[SentenceGraph] = 
        for {
          a <- as
          b <- bs
        } yield a union b

      def link(leftIndex: Int, rightIndex: Int, l: List[LinkRule.LeftLink], r: List[LinkRule.RightLink]): List[SentenceGraph] = {
        if(leftIndex + 1 == rightIndex) {
          if(l.isEmpty && r.isEmpty) {
            List(ImmutableSentenceGraph.empty)
          } else {
            List()
          }
        } else {
          val links = collection.mutable.ListBuffer[SentenceGraph]()

          for(w <- leftIndex + 1 until rightIndex) {
            for(d <- rules(w).disjunction) {
              val leftLinks = product(makeLink(leftIndex, w, r, d.leftLinks), link(leftIndex, w, d.leftLinks.tail, r.tail))
              val rightLinks = product(makeLink(w, rightIndex, d.rightLinks, l), link(w, rightIndex, l.tail, d.rightLinks.tail))

              if(!leftLinks.isEmpty) {
                links ++= product(leftLinks, link(w, rightIndex, l, d.rightLinks))
              }

              if(!rightLinks.isEmpty) {
                links ++= product(rightLinks, link(leftIndex, w, d.leftLinks, r))
              }

              links ++= product(leftLinks, rightLinks)
            }
          }

          links.toList
        }
      }

      def conjunctParses(start: Int, conjunct: LinkRule.LinkList): List[SentenceGraph] = {
        if(conjunct.leftLinks.isEmpty) {
          if(conjunct.rightLinks.isEmpty && start < words.length) {
            rules(start + 1).disjunction.map { conjunct2 =>
              conjunctParses(start + 1, conjunct2)
            } .reduce(_ ++ _)
          } else {
            link(start, words.length, List.empty, conjunct.rightLinks)
          }
        } else {
          List.empty
        }
      }
 
      rules(0).disjunction.map { conjunct =>
        conjunctParses(0, conjunct)
      } .reduce(_ ++ _)
    }
  }
}
