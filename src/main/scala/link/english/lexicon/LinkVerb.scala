package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class LinkVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  val activeVerbWithPredicate =
    opt(l(A)) & (r(P) | r(B))

  def linkRules: List[(String, LinkRule.NormalForm)] =
    List(
      root ->
        ((opt(l(Hr)) & l(W) & activeVerbWithPredicate) | 
         (l(Ss) & l(Hs) & activeVerbWithPredicate) | 
         (l(Sp) & l(Hp) & activeVerbWithPredicate) | 
         (l(Hp) & l(Sp) & activeVerbWithPredicate) | 
         (l(Hs) & l(Ss) & activeVerbWithPredicate)), // TODO - opt(l("Hr")) & l("W") ...
      presentSingular -> 
        (opt(l(A)) & l(Ss) & activeVerbWithPredicate),
      presentPlural -> 
        (opt(l(A)) & l(Sp) & activeVerbWithPredicate),
      past -> 
        (opt(l(A)) & l(S) & activeVerbWithPredicate),
      presentParticiple ->
        (l(Tr) & r(O)))

  def words = List(root, presentSingular, presentPlural, presentParticiple, past, pastParticiple)

  def wordTags = List( ) // TODO - STUB
}
