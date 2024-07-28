package link.english

import link.english.lexicon.{Preposition, Direction}

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
    Preposition("outside"),

    Direction("north"),
    Direction("south"),
    Direction("east"),
    Direction("west"),
    Direction("up"),
    Direction("down"),
    Direction("left"),
    Direction("right"),
    ))
}
