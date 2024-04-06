package link.english.lexicon

import link.tokenizer.Tokenizer

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case object Wall extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def linkRules: List[(String, LinkRule.NormalForm)] =
    List(Tokenizer.LEFT_WALL -> opt(r(W)))

  def words = List() // NB Not a word that the tokenizer _finds_. It puts it in there on its own

  def wordTags = List.empty // STUB?
}

