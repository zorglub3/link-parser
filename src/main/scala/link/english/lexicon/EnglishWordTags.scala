package link.english.lexicon

import link.rule.WordTag

object EnglishWordTags {
  // word classes
  case object Noun extends WordTag("noun")
  case object Verb extends WordTag("verb")
  case object Adjective extends WordTag("adjective")
  case object Preposition extends WordTag("preposition")
  case object Pronoun extends WordTag("pronoun")
  case object Determiner extends WordTag("determiner")

  // pronoun person
  case class Person(person: Int) extends WordTag(s"person-$person")
  
  // verb
  case object Transitive extends WordTag("transitive")
  case object Intransitive extends WordTag("intransitive")

  // case
  case object Nominative extends WordTag("nominative")
  case object Accusative extends WordTag("accusative")
  case object Possessive extends WordTag("possessive")

  // number
  case object Singular extends WordTag("singular")
  case object Plural extends WordTag("plural")

  // tense
  case object Root extends WordTag("root")
  case object Present extends WordTag("present")
  case object Past extends WordTag("past")

  // adjective
  case object Absolute extends WordTag("absolute")
  case object Comparative extends WordTag("comparative")
  case object Superlative extends WordTag("superlative")
}
