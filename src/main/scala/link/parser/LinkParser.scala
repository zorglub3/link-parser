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

    // TODO clean up this mess...
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
          List(ParseLinkError(List( words(leftIndex) -> leftIndex)))
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
        Left(ParseLinkError(List.empty))
      } else if(!results.isEmpty) {
        Right(results)
      } else {
        Left(failures.reduce(_ mergeFailure _))
      }
    }

    processOutcomes(wallStart ++ normStart)
  }

  def apply(words: Vector[W]): Either[ParseFailure[W], List[ParseResult[W]]] = {
    links(words)
  }

  import cats.data.StateT
  import cats.Applicative
  import cats.Alternative
  import cats.syntax.all._
  import _root_.link.graph.ImmutableSentenceGraph
  import _root_.link.graph.SentenceEdge
  import _root_.link.graph.SentenceEdgeSyntax

  type Parse[A] = StateT[List, ParseState, A]
  case class ParseState(
    words: Vector[W],
    wordTags: Vector[List[WordTag]],
    graph: ImmutableSentenceGraph.T,
  ) {
    def parseResult(): ParseResult[W] =
      ParseResult(
        graph,
        words,
        wordTags)
  }

  def initState(words: Vector[W]): ParseState =
    ParseState(
      words,
      Vector.fill(words.length)(List.empty),
      ImmutableSentenceGraph.empty)

  def get: Parse[ParseState] = StateT.get

  def set(s: ParseState): Parse[Unit] = StateT.set(s)

  def success: Parse[Unit] = StateT.pure( () )

  def fail: Parse[Unit] = StateT.liftF(List.empty) 

  def liftF[A](l: List[A]): Parse[A] = StateT.liftF(l) 

  def pure[A](v: A): Parse[A] = StateT.pure(v)

  def liftOption[A](a: Option[A]): Parse[A] = StateT.liftF(a.toList)

  def wordRuleEntry(w: Int): Parse[RuleMap.Entry] = {
    for {
      parseState <- get
      entry <- liftF(ruleMap.lookup(parseState.words(w)))
    } yield entry
  }

  def guard(b: Boolean): Parse[Unit] = StateT.liftF(if(b) List( () ) else List.empty)

  def addLink(edge: SentenceEdge): Parse[Unit] = 
    StateT.modify { parseState => parseState.copy(graph = parseState.graph + edge) }

  def tagWord(w: Int, tags: List[WordTag]): Parse[Unit] = {
    StateT.modify { parseState => 
      val newTags = Vector.tabulate(parseState.words.length) { n =>
        if(n == w) { tags ++ parseState.wordTags(n) } else { parseState.wordTags(n) }
      }
      parseState.copy(wordTags = newTags)
    }
  }

  implicit class ParseSyntax[A](a: Parse[A]) {
    def orElse(b: Parse[A]): Parse[A] = {
      val sb = for {
        bb <- b
        s <- get
      } yield (s, bb)

      for {
        state <- get
        f <- liftF(a.runF)
        p = f.apply(state)
        p <- if(p.nonEmpty) liftF(p) else sb
        _ <- set(p._1)
      } yield p._2
    }
  }
      
  def where(cond: Boolean)(p: Parse[Unit]): Parse[Unit] = {
    if(cond) p else pure(())
  }

  def makeLink(
    word1: Int, 
    word2: Int, 
    as: List[LinkRule.RightLink], 
    bs: List[LinkRule.LeftLink]
  ): Parse[Unit] = {
    import SentenceEdgeSyntax._

    for {
      a <- liftOption(as.headOption)
      b <- liftOption(bs.headOption)
      _ <- guard(a.linkTag.matches(b.linkTag))
      _ <- addLink(word1 ~ word2 :+ a.linkTag.simplify)
    } yield ()
  }

  def link(
    leftIndex: Int, 
    rightIndex: Int, 
    l: List[LinkRule.LeftLink], 
    r: List[LinkRule.RightLink]
  ): Parse[Unit] = {
    if(leftIndex + 1 == rightIndex) {
      if(l.isEmpty && r.isEmpty) { success } else { fail }
    } else {
      def leftLinks(w: Int, linkRule: LinkRule.LinkList): Parse[Boolean] = {
        for {
          _ <- makeLink(leftIndex, w, r, linkRule.leftLinks)
          _ <- link(leftIndex, w, linkRule.leftLinks.tail, r.tail)
        } yield true 
      }

      def rightLinks(w: Int, linkRule: LinkRule.LinkList): Parse[Boolean] = {
        for {
          _ <- makeLink(w, rightIndex, linkRule.rightLinks, l)
          _ <- link(w, rightIndex, l.tail, linkRule.rightLinks.tail)
        } yield true 
      }

      for {
        w <- liftF((leftIndex + 1 until rightIndex).toList)
        entry <- wordRuleEntry(w)
        tags = entry.wordTags
        linkRule = entry.linkRule
        hasLeftLinks <- leftLinks(w, linkRule) orElse pure(false)
        hasRightLinks <- rightLinks(w, linkRule) orElse pure(false)
        _ <- where(hasLeftLinks) { link(w, rightIndex, l, linkRule.rightLinks) }
        _ <- where(hasRightLinks) { link(leftIndex, w, linkRule.leftLinks, r) }
        _ <- guard(hasLeftLinks || hasRightLinks)
        _ <- tagWord(w, tags)
      } yield () 
    }
  }

  def begin(start: Int, end: Int): Parse[Unit] = {
    for {
      r <- wordRuleEntry(start)
      _ <- guard(r.linkRule.l.isEmpty)
      _ <- link(start, end, List.empty, r.linkRule.rightLinks)
      _ <- tagWord(start, r.wordTags)
    } yield ()
  }

  def parse(words: Vector[W]): List[ParseResult[W]] =
    (begin(0, words.length) orElse begin(1, words.length)).runS(initState(words)).map(_.parseResult())
}
