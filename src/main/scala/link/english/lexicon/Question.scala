package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class Question() extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def linkRules: List[(String, LinkRule.NormalForm)] =
    List(
      "who" -> (l(W) & r(Ss)),
      "what" -> (l(W) & r(Ss)),
      "where" -> (l(W) & r(Q)),
      "why" -> (l(W) & r(Q)),
      "when" -> (l(W) & r(Q)),
      "how" -> (l(W) & opt(r(A)) & r(Q)))

  def words = List("who", "what", "where", "why", "when", "how")
}


