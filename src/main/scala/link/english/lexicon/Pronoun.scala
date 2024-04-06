package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class PersonalPronoun(
  nominative: String,
  accusative: String,
  possessive: String,
  person: Int, // 1 2 or 3
  isPlural: Boolean
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def nominativeRule = 
    if(isPlural) {
      (r(Spp) | l(Sq("pp")))
    } else { 
      person match {
        case 1 => (r(Spi) | l(Sq("pi")))
        case 2 => (r(Spp) | l(Sq("pp")))
        case 3 => (r(Ss) | l(Sq("s")))
        case _ => ???
      }
    }

  def linkRules: List[(String, LinkRule.NormalForm)] = 
    List(
      nominative -> nominativeRule,
      accusative -> (l(O) | l(R))) // TODO possessive

  def words = List(nominative, accusative, possessive)

  def baseTags = List(Pronoun, Person(person)) :+ (if(isPlural) Plural else Singular)
  
  def wordTags = List(
    nominative -> (Nominative :: baseTags),
    accusative -> (Accusative :: baseTags),
    possessive -> (Possessive :: baseTags),
  )
}

case class DemonstrativePronoun(
  singular: String,
  plural: String,
  isFar: Boolean
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def linkRules: List[(String, LinkRule.NormalForm)] =
    List() // STUB

  def words = List(singular, plural)

  def wordTags = List.empty // STUB
}

