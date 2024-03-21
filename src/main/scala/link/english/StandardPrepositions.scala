package link.english

import link.english.lexicon.Preposition

trait StandardPrepositions { self: EnglishLexiconBuilder =>
  addEntries(List(
    Preposition("under"),
    Preposition("next_to"),
    Preposition("on_top_of"),
    Preposition("into"),
    Preposition("among"),
    Preposition("opposite"),

    Preposition("at"),
    Preposition("in"),
    Preposition("on"),

    Preposition("along"),
    Preposition("across"),
    Preposition("behind"),
    Preposition("within"),
    Preposition("outside")))
}
