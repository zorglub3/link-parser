package link.english

import link.rule._
import link.english.lexicon.EnglishLexiconEntry
import link.tokenizer.{TokenLexicon, StringTokenLexiconBuilder}

class EnglishLexiconBuilder {
  lazy val entries = List.newBuilder[EnglishLexiconEntry]

  def addEntries(es: List[EnglishLexiconEntry]): Unit =
    entries ++= es

  def tokenLexicon: TokenLexicon[String] = {
    val builder = new StringTokenLexiconBuilder

    for(e <- entries.result()) e.words.foreach(builder.addToken)

    for(e <- entries.result()) e.wordTags.foreach { p => 
      p._2.foreach(builder.addTokenTag(p._1, _))
    }

    builder.result
  }

  def wordBook: WordBook = {
    new WordBook(entries.result())    
  }

  def linkRules: Map[String, LinkRule.NormalForm] = {
    val linkRuleSet = new LinkRuleSet[String] {}

    for {
      e <- entries.result()
      (word, rule) <- e.linkRules.toList
    } linkRuleSet.addLinkRule(word, rule)

    linkRuleSet.wordRules.toMap
  }

  def ruleMap: RuleMap[String] = {
    val ruleMapResult: RuleMap[String] = new RuleMap

    for {
      e <- entries.result()
      (word, rule) <- e.linkRules.toList
    } {
      val tags = e.wordTags.filter(_._1 == word).map(_._2).flatten
      ruleMapResult.addEntry(word, tags, List(rule))
    }

    ruleMapResult
  }
}
