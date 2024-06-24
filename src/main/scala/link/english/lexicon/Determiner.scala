package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class Determiner(word: String, isPlural: Boolean) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val wordEntries = 
    List(
      WordEntry(
        word,
        List(EnglishWordTags.Determiner),
        if(isPlural) { r(Dp) } else { r(Ds) },
      )
    )
}

case class PossessiveDeterminer(word: String, person: Int, isPlural: Boolean) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags._

  val wordEntries = 
    List(
      WordEntry(
        word,
        List(EnglishWordTags.Determiner, Pronoun, Person(person), if(isPlural) { Plural } else { Singular}, Possessive),
        r(D),
      )      
    )
}
