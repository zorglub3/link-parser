package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax, WordTag}
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class Noun(
  singular: String,
  plural: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  val singularNoun = opt(l(J)) & l(Ds) & opt(r(P))
  val pluralNoun = opt(l(J)) & opt(l(Dp)) & opt(r(P))

  val singularNounVerb =
    l(Sq("s")) | r(Ss) | l(O)
  val pluralNounVerb =
    l(Sq("p")) | r(Ss) | l(O)
    
  def linkRules: List[(String, LinkRule.NormalForm)] =
    List(
      singular -> 
        ((singularNoun & singularNounVerb) | (singularNoun & l(R)) | (singularNoun & l(W))),
      plural -> 
        ((pluralNoun & pluralNounVerb) | (pluralNoun & l(R)) | (pluralNoun & l(W))))

  def words = List(singular, plural)

  def wordTags: List[(String, List[WordTag])] = List(
    singular -> List(EnglishWordTags.Noun, Singular),
    plural -> List(EnglishWordTags.Noun, Plural))
}
