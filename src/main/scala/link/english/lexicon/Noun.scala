package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class Noun(
  singular: String,
  plural: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  val singularNoun = opt(l(J)) & l(Ds) & opt(r(P))
  val pluralNoun = opt(l(J)) & opt(l(Dp)) & opt(r(P))

  def linkRules: List[(String, LinkRule.NormalForm)] =
    List(
      singular -> ((singularNoun & l(Sq("s"))) | (singularNoun & r(Ss)) | (singularNoun & l(O)) | (singularNoun & l(R)) | (singularNoun & l(W))),
      plural -> ((pluralNoun & l(Sq("p"))) | (pluralNoun & r(Sp)) | (pluralNoun & l(O)) | (pluralNoun & l(R)) | (pluralNoun & l(W))))

  def words = List(singular, plural)
}
