package link.english

import link.english.lexicon._

trait StandardWords { self: EnglishLexiconBuilder =>
  addEntries(List(
    Wall,

    PersonalPronoun("i", "me", "mine", 1, false),
    PersonalPronoun("you", "you", "your", 2, false),
    PersonalPronoun("he", "him", "his", 3, false),
    PersonalPronoun("she", "her", "hers", 3, false),
    PersonalPronoun("it", "it", "its", 3, false),
    PersonalPronoun("we", "us", "our", 1, true),
    PersonalPronoun("you", "you", "your", 2, true),
    PersonalPronoun("they", "them", "their", 3, true),

    DemonstrativePronoun("this", "these", true),
    DemonstrativePronoun("that", "those", false),

    Determiner("the", false),
    Determiner("the", true),
    Determiner("a", false),
    Determiner("an", false),
    Determiner("many", true),
    Determiner("all", true),
    Determiner("all_the", true),
    Determiner("some", true),
    Determiner("some", false),

    PossessiveDeterminer("my", 1, false),
    PossessiveDeterminer("your", 2, false),
    PossessiveDeterminer("his", 3, false),
    PossessiveDeterminer("her", 3, false),
    PossessiveDeterminer("its", 3, false),
    PossessiveDeterminer("our", 1, true),
    PossessiveDeterminer("your", 2, true),
    PossessiveDeterminer("their", 3, true),

    Preposition("to"),
    Preposition("from"),
    Preposition("over"),
    Preposition("under"),
    Preposition("with"),
    Preposition("on"),
    Preposition("like"),

    Question()
  ))

  val abbreviatoins = Map(
    "isnt" -> List("is", "not"),
    "doesnt" -> List("does", "not"),
    "didnt" -> List("did", "not"),
    "dont" -> List("do", "not"),
    "arent" -> List("are", "not"),
    "im" -> List("i", "am"),
    "havent" -> List("have", "not"),
    "hasnt" -> List("has", "not"))
}
