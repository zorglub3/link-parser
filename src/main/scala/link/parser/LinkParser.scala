package link.parser

import link.rule._

class LinkParser[W](val ruleMap: RuleMap[W]) {
  def ruleMapEntries(sentence: Vector[W]): Vector[List[RuleMap.Entry]] =
    sentence.map(ruleMap.lookup)
    
  def check(words: Vector[W]): Int = {
    val rules = words.map(ruleMap.lookupLinkRules _)
      
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

  def links(words: Vector[W]): List[ParseResult[W]] = {
    val wordRuleEntries = words.map(ruleMap.lookup _)

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
          for(entry <- wordRuleEntries(w)) {
            val localLinks = collection.mutable.ListBuffer[ParseResult[W]]()

            val tags = entry.wordTags
            val linkRule = entry.linkRule
            
            val leftLinks = 
              product(
                makeLink(leftIndex, w, r, linkRule.leftLinks), 
                link(leftIndex, w, linkRule.leftLinks.tail, r.tail))

            val rightLinks = 
              product(
                makeLink(w, rightIndex, linkRule.rightLinks, l), 
                link(w, rightIndex, l.tail, linkRule.rightLinks.tail))

            if(!leftLinks.isEmpty) {
              localLinks ++= product(leftLinks, link(w, rightIndex, l, linkRule.rightLinks))
            }

            if(!rightLinks.isEmpty) {
              localLinks ++= product(rightLinks, link(leftIndex, w, linkRule.leftLinks, r))
            }

            localLinks ++= product(leftLinks, rightLinks)

            links ++= localLinks.toList.map(_.tagWord(w, tags))
          }
        }

        links.toList
      }
    }

    def conjunctParses(start: Int, wordTags: List[WordTag], linkRule: LinkRule.LinkList): List[ParseResult[W]] = {
      if(linkRule.leftLinks.isEmpty) {
        if(linkRule.rightLinks.isEmpty && start < words.length) {
          wordRuleEntries(start + 1).map { case RuleMap.Entry(wordTags, linkRule) =>
            conjunctParses(start + 1, wordTags, linkRule)
          } .reduce(_ ++ _)
        } else {
          link(start, words.length, List.empty, linkRule.rightLinks).map(_.tagWord(start, wordTags))
        }
      } else {
        List.empty
      }
    }
 
    wordRuleEntries(0).map { case RuleMap.Entry(wordTags, linkRule) =>
      conjunctParses(0, wordTags, linkRule)
    } .reduce(_ ++ _)
  }
}
