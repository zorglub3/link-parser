package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class Question() extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries =
    List(
      WordEntry(
        "who",
        List(EnglishWordTags.Question),
        l(W) & r(Ss),
      ),
      WordEntry(
        "what",
        List(EnglishWordTags.Question),
        l(W) & r(Ss),
      ),
      WordEntry(
        "where",
        List(EnglishWordTags.Question),
        l(W) & r(Q),
      ),
      WordEntry(
        "why",
        List(EnglishWordTags.Question),
        l(W) & r(Q),
      ),
      WordEntry(
        "when",
        List(EnglishWordTags.Question),
        l(W) & r(Q),
      ),
      WordEntry(
        "how",
        List(EnglishWordTags.Question),
        l(W) & opt(r(A)) & r(Q),
      ),
    )
}


