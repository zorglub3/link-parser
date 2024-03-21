package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class Determiner(word: String, isPlural: Boolean) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def linkRules: List[(String, LinkRule.NormalForm)] = {
    if(isPlural) {
      List(word -> r(Dp))
    } else {
      List(word -> r(Ds))
    }
  }

  def words = List(word)
}

case class PossessiveDeterminer(word: String, person: Int, isPlural: Boolean) extends EnglishLexiconEntry {
  def linkRules: List[(String, LinkRule.NormalForm)] = List.empty
  def words = List(word)
}
