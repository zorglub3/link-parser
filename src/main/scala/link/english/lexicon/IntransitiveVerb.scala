package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class IntransitiveVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  val activeVerb = opt(l(A)) & opt(r(A)) & opt(r(P))

  def linkRules: List[(String, LinkRule.NormalForm)] = 
    List(
      root -> ((opt(l(Hr)) & l(W) & activeVerb) | (l(Hs) & l(Ss) & activeVerb) | (l(Hp) & l(Sp) & activeVerb) | (l(Sp) & l(Hp) & activeVerb) | (l(Ss) & l(Hs) & activeVerb)),
      presentSingular -> (opt(l(A)) & l(Ss) & activeVerb),
      presentPlural -> (opt(l(A)) & l(Sp) & activeVerb),
      past -> (opt(l(A)) & l(S) & activeVerb),
      presentParticiple -> l(Tr),
      pastParticiple -> l(Ta))

  def words = List(root, presentSingular, presentPlural, presentParticiple, past, pastParticiple)
}
