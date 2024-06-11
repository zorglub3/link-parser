package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class PersonalPronoun(
  nominative: String,
  accusative: String,
  possessive: String,
  person: Int, // 1 2 or 3
  isPlural: Boolean,
  genderOpt: Option[link.rule.WordTag],
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

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

  def gender = genderOpt.toList

  def baseTags = gender ++ List(Pronoun, Person(person)) :+ (if(isPlural) Plural else Singular)

  val wordEntries =
    List(
      WordEntry(
        nominative,
        Nominative :: baseTags,
        nominativeRule,
      ),
      WordEntry(
        accusative,
        Accusative :: baseTags,
        l(O) | l(R),        
      ),
    )
}

case class DemonstrativePronoun(
  singular: String,
  plural: String,
  isFar: Boolean
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  def nominativeSingular = r(Ss) | l(Sq("s"))
  def nominativePlural = r(Spp) | l(Sq("pp"))
  def accusative = l(O) | l(R)

  val wordEntries =
    List(
      WordEntry(
        singular,
        List(Pronoun, Demonstrative, Singular, Nominative),
        nominativeSingular,
      ),
      WordEntry(
        singular,
        List(Pronoun, Demonstrative, Singular, Accusative),
        accusative,
      ),
      WordEntry(
        plural,
        List(Pronoun, Demonstrative, Plural, Nominative),
        nominativePlural,
      ),
      WordEntry(
        plural,
        List(Pronoun, Demonstrative, Plural, Accusative),
        accusative,
      )
    )
}

