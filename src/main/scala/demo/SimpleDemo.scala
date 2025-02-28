package demo

import link.LinkError
import link.tokenizer._
import link.parser.LinkParser
import link.english._
import link.english.lexicon.EnglishLinkTags
import link.rule._
// import link.english.writer.EnglishWriter

class SimpleDemo {
  def linkRules() = new LinkRuleSet[String] {
    import EnglishLinkTags._
    import LinkRuleSyntax._

    "the" :- r(D)
    "dog" :- (l(D) & r(Ss))
    "dogs" :- (opt(l(D)) & r(Sp))
    "runs" :- l(Ss)
    "run"  :- l(Sp)
    "ran"  :- (l(Ss) | l(Sp))
    "////" :- opt(r(W))
  }

  val tokenLexiconBuilder = new StringTokenLexiconBuilder
  List("the", "dog", "dogs", "run", "runs", "ran").foreach { w => 
    tokenLexiconBuilder.addToken(w)
  }

  val tokenLexicon = tokenLexiconBuilder.result
  val tokenizer = new Tokenizer[String](tokenLexicon, " ")
  val parser = new LinkParser[String](linkRules().makeRuleMap)

  implicit class LinkErrorSyntax[E <: link.LinkError, T](v: Either[E, T]) {
    def coerce: Either[link.LinkError, T] = v
  }

  def parseLinks(str: String) = {
    for {
      tokens <- tokenizer(str.toLowerCase).coerce
      results = parser.parse(tokens)
    } yield results
  }
}
