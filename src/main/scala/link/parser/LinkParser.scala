package link.parser

import cats.implicits._
import link.rule._

class LinkParser[W](val ruleMap: RuleMap[W]) {
  def sentenceRules(sentence: Vector[W]): Option[Vector[LinkRule.NormalForm]] = 
    sentence.traverse(ruleMap.lookupLinkRules _)

  def check(words: Vector[W]): Int = {
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

  def links(words: Vector[W]): List[ParseResult[W]] = {
    sentenceRules(words).fold(List.empty[ParseResult[W]]) { rules =>
      def makeLink(w1: Int, w2: Int, as: List[LinkRule.RightLink], bs: List[LinkRule.LeftLink]): List[ParseResult[W]] = 
        (as, bs) match {
          case (a :: _, b :: _) if a.linkTag.matches(b.linkTag) => {
            List(ParseResult.withLink(words, w1, w2, a.linkTag))
          }
          case _ => List.empty
        }

      def product(as: List[ParseResult[W]], bs: => List[ParseResult[W]]): List[ParseResult[W]] = 
        for {
          a <- as
          b <- bs
        } yield a merge b

      def link(leftIndex: Int, rightIndex: Int, l: List[LinkRule.LeftLink], r: List[LinkRule.RightLink]): List[ParseResult[W]] = {
        if(leftIndex + 1 == rightIndex) {
          if(l.isEmpty && r.isEmpty) {
            List(ParseResult.emptyFromWords(words))
          } else {
            List()
          }
        } else {
          val links = collection.mutable.ListBuffer[ParseResult[W]]()

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

      def conjunctParses(start: Int, conjunct: LinkRule.LinkList): List[ParseResult[W]] = {
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
