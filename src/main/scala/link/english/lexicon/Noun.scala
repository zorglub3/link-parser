package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class Noun(
  singular: String,
  plural: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val singularNoun = opt(l(J)) & l(Ds) & opt(r(P))
  val pluralNoun = opt(l(J)) & opt(l(Dp)) & opt(r(P))

  val singularNounVerb =
    l(Sq("s")) | r(Ss) | l(O)
  val pluralNounVerb =
    l(Sq("p")) | r(Ss) | l(O)
    
  val wordEntries = 
    List(
      WordEntry(
        singular,
        List(EnglishWordTags.Noun, Singular, NounRoot(singular)),
        ((singularNoun & singularNounVerb) | (singularNoun & l(R)) | (singularNoun & l(W))),
      ),
      WordEntry(
        plural,
        List(EnglishWordTags.Noun, Plural, NounRoot(singular)),
        ((pluralNoun & pluralNounVerb) | (pluralNoun & l(R)) | (pluralNoun & l(W))),
      ),
    )
}
