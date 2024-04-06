package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class Preposition(word: String) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def linkRules: List[(String, LinkRule.NormalForm)] = List(word -> (l(P) & r(R)))

  def words = List(word)

  def wordTags = List(
    word -> List(EnglishWordTags.Preposition)
  )
}
