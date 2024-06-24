package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class SimpleAdjective(
  absolute: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries =
    List(
      WordEntry( 
        absolute, 
        List(EnglishWordTags.Adjective, EnglishWordTags.Absolute, EnglishWordTags.AdjectiveRoot(absolute)),
        (l(B) | (opt(l(J)) & r(J)))
      ),
    )
}
