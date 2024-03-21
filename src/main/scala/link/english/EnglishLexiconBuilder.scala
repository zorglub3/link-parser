package link.english

import link.rule._
import link.english.lexicon.EnglishLexiconEntry
import link.tokenizer.{TokenLexicon, StringTokenLexiconBuilder}

import scala.collection.mutable.HashMap

class EnglishLexiconBuilder {
  lazy val entries = List.newBuilder[EnglishLexiconEntry]

  def addEntries(es: List[EnglishLexiconEntry]): Unit =
    entries ++= es

  def tokenLexicon: TokenLexicon[String] = {
    val builder = new StringTokenLexiconBuilder
    for(e <- entries.result) e.words.foreach(builder.addToken)
    builder.result
  }

  def linkRules: Map[String, LinkRule.NormalForm] = {
    val linkRuleSet = new LinkRuleSet[String] {}

    for {
      e <- entries.result
      (word, rule) <- e.linkRules.toList
    } linkRuleSet.addLinkRule(word, rule)

    linkRuleSet.wordRules.toMap
  }
}
