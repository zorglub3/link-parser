package link.english.lexicon

import link.tokenizer.Tokenizer

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case object Wall extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries = 
    List(
      WordEntry(
        Tokenizer.LEFT_WALL,
        List(EnglishWordTags.Wall),
        opt(r(W)),
      )
    )
}

