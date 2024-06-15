package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class Negation() extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries =
    List(
      WordEntry(
        "not",
        List(EnglishWordTags.Negation),
        l(N) | r(N),
      )
    )
}
