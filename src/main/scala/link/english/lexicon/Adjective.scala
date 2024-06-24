package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

// TODO adjectives where comparative and superlative are made with "more" and "most"
case class Adjective(
  absolute: String,
  comparative: String,
  superlative: String,
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
      WordEntry(
        superlative,
        List(EnglishWordTags.Adjective, EnglishWordTags.Superlative, EnglishWordTags.AdjectiveRoot(absolute)),
        (l(B) | (opt(l(J)) & r(J)))
      ),
      // TODO comparative form
    )
}
