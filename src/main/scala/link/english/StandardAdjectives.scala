package link.english

import link.english.lexicon._

trait StandardAdjectives { self: EnglishLexiconBuilder =>
  addEntries(List(
    Adjective("small", "smaller", "smallest"),
    Adjective("big", "bigger", "biggest"),
    Adjective("tall", "taller", "tallest"),
    Adjective("warm", "warmer", "warmest"),
    Adjective("cold", "colder", "coldest"),
    Adjective("dark", "darker", "darkest"),
    Adjective("light", "lighter", "lightest"),
  ))  
}
