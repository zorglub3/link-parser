package link.parser

import link.rule._

// TODO fix ParseFailure - data is not so useful
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

  def links(words: Vector[W]): Either[ParseFailure[W], List[ParseResult[W]]] = {
    val wordRuleEntries = words.map(ruleMap.lookup _)

    def makeLink(w1: Int, w2: Int, as: List[LinkRule.RightLink], bs: List[LinkRule.LeftLink]): List[ParseOutcome[W]] = 
      (as, bs) match {
        case (a :: _, b :: _) if a.linkTag.matches(b.linkTag) => {
          List(ParseResult.withLink(words, w1, w2, a.linkTag))
        }
        case _ => List.empty
      }

    def product(as: List[ParseOutcome[W]], bs: => List[ParseOutcome[W]]): List[ParseOutcome[W]] = 
      for {
        a <- as
        b <- bs
      } yield a merge b

    def link(leftIndex: Int, rightIndex: Int, l: List[LinkRule.LeftLink], r: List[LinkRule.RightLink]): List[ParseOutcome[W]] = {
      if(leftIndex + 1 == rightIndex) {
        if(l.isEmpty && r.isEmpty) {
          List(ParseResult.emptyFromWords(words))
        } else {
          List(ParseFailure(List( words(leftIndex) -> leftIndex)))
        }
      } else {
        val links = collection.mutable.ListBuffer[ParseOutcome[W]]()

        for(w <- leftIndex + 1 until rightIndex) {
          for(entry <- wordRuleEntries(w)) {
            val localLinks = collection.mutable.ListBuffer[ParseOutcome[W]]()

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

    def parseFrom(i: Int) = 
      wordRuleEntries(i)
        .filter( _.linkRule.l.isEmpty)
        .map { case RuleMap.Entry(wordTags, linkRule) =>
          link(i, words.length, List.empty, linkRule.rightLinks)
            .map(_.tagWord(i, wordTags))
        } .flatten

    val wallStart = parseFrom(0)
    val normStart = if(words.length > 1) { parseFrom(1) } else { List.empty }

    def processOutcomes(outcomes: List[ParseOutcome[W]]): Either[ParseFailure[W], List[ParseResult[W]]] = {
      val results: List[ParseResult[W]] = outcomes.collect { case pr: ParseResult[W] => pr } 
      val failures: List[ParseFailure[W]] = outcomes.collect { case pf: ParseFailure[W] => pf }

      if(results.isEmpty && failures.isEmpty) {
        Left(ParseFailure(List.empty))
      } else if(!results.isEmpty) {
        Right(results)
      } else {
        Left(failures.reduce(_ mergeFailure _))
      }
    }

    processOutcomes(wallStart ++ normStart)
  }
}
