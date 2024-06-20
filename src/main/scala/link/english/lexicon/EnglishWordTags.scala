package link.english.lexicon

import link.rule.WordTag

object EnglishWordTags {
  // word classes
  case object Noun extends WordTag("noun")
  case object Verb extends WordTag("verb")
  case object Adjective extends WordTag("adjective")
  case object Adverb extends WordTag("adverb")
  case object Preposition extends WordTag("preposition")
  case object Pronoun extends WordTag("pronoun")
  case object Determiner extends WordTag("determiner")
  case object Question extends WordTag("question")
  case object Wall extends WordTag("wall")
  case object ProperNoun extends WordTag("propernoun")
  case object Negation extends WordTag("negation")

  // pronoun specific
  case class Person(person: Int) extends WordTag("pronounperson")
  case object Demonstrative extends WordTag("demonstrative")
  case object MaleGender extends WordTag("malegender")
  case object FemaleGender extends WordTag("femalegender")
  case object NeuterGender extends WordTag("neutergender")
  
  // verb
  case object Transitive extends WordTag("transitive")
  case object Intransitive extends WordTag("intransitive")
  case object LinkVerb extends WordTag("linkverb")
  case object HelpVerb extends WordTag("helpverb")
  case object ToBe extends WordTag("tobe")

  // case
  case object Nominative extends WordTag("nominative")
  case object Accusative extends WordTag("accusative")
  case object Possessive extends WordTag("possessive")

  // number
  case object Singular extends WordTag("singular")
  case object Plural extends WordTag("plural")

  // noun root
  case class NounRoot(root: String) extends WordTag("nounroot")

  // tense
  case object Root extends WordTag("root")
  case object Present extends WordTag("present")
  case object Past extends WordTag("past")
  case object PresentParticiple extends WordTag("presentparticiple")
  case object PastParticiple extends WordTag("pastparticiple")
  case class VerbRoot(root: String) extends WordTag("verbroot")

  // adjective
  case class AdjectiveRoot(root: String) extends WordTag("adjectiveroot")
  case object Absolute extends WordTag("absolute")
  case object Comparative extends WordTag("comparative")
  case object Superlative extends WordTag("superlative")
}
