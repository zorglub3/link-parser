package link.english

import link.english.lexicon._

trait StandardAdverbs { self: EnglishLexiconBuilder =>
  addEntries(List(
    Adverb("quickly"),
    Adverb("slowly"),
    Adverb("silently"),
    Adverb("yesterday"),
    Adverb("today"),
    Adverb("tomorrow"),
    Adverb("now"),
    Adverb("soon"),
    Adverb("abruptly"),
    Adverb("boldly"),
  ))
}
