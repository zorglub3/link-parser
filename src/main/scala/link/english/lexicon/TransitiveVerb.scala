package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class TransitiveVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  val activeVerbWithObject = opt(l(A)) & r(O) & opt(r(A)) & opt(r(P))

  def linkRules: List[(String, LinkRule.NormalForm)] = 
    List(
      root -> 
        ((opt(l(Hr)) & l(W) & activeVerbWithObject) | 
         (l(Ss) & l(Hs) & activeVerbWithObject) | 
         (l(Sp) & l(Hp) & activeVerbWithObject) | 
         (l(Hp) & l(Sp) & activeVerbWithObject) | 
         (l(Hs) & l(Ss) & activeVerbWithObject)), // TODO - opt(l("Hr")) & l("W") ...
      presentSingular -> 
        (opt(l(A)) & l(Ss) & activeVerbWithObject),
      presentPlural -> 
        (opt(l(A)) & l(Sp) & activeVerbWithObject),
      past -> 
        (opt(l(A)) & l(S) & activeVerbWithObject),
      presentParticiple ->
        (l(Tr) & r(O)))

  def words = List(root, presentSingular, presentPlural, presentParticiple, past, pastParticiple)
}
