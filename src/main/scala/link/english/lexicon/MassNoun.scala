package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

// your regular count nouns such as chair, rock or glass
case class MassNoun(
  singular: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val singularNoun = opt(l(J)) & l(Ds) & opt(r(P))
  val pluralNoun = opt(l(J)) & opt(l(Dp)) & opt(r(P))

  val singularNounVerb =
    l(Sq("s")) | r(Ss) | l(O)
  val pluralNounVerb =
    l(Sq("p")) | r(Sp) | l(O)
    
  val wordEntries = 
    List(
      WordEntry(
        singular,
        List(EnglishWordTags.Noun, Singular, NounRoot(singular), EnglishWordTags.MassNoun),
        ((singularNoun & singularNounVerb) | (singularNoun & l(R)) | (singularNoun & l(W))),
      ),
    )
}
