package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

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

