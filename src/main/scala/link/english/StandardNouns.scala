package link.english

import link.english.lexicon.Noun

trait StandardNouns { self: EnglishLexiconBuilder =>
  addEntries(List(
    Noun("table", "tables"),
    Noun("chair", "chairs"),
    Noun("house", "houses"),
    Noun("road", "roads"),
    Noun("knife", "knives"),
    Noun("fork", "forks"),
    Noun("plate", "plates"),
    Noun("man", "men"),
    Noun("woman", "women"),
    Noun("telescope", "telescopes"),
    Noun("drink", "drinks"),
  ))
}
