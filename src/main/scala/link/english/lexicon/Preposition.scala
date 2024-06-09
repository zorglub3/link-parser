package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class Preposition(word: String) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries =
    List(
      WordEntry(
        word,
        List(EnglishWordTags.Preposition),
        (l(P) & r(R)),
      ),
    )
}
