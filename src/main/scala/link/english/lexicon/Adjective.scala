package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

// TODO adjectives where comparative and superlative are made with "more" and "most"
case class Adjective(
  absolute: String,
  comparative: String,
  superlative: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  def linkRules =
    List(
      absolute -> (l(B) | r(J)),
      superlative -> (l(B) | r(J))
    ) // TODO: comparative

  def words = List(absolute, comparative, superlative)

  def wordTags = List(
    absolute -> List(EnglishWordTags.Adjective, EnglishWordTags.Absolute),
    comparative -> List(EnglishWordTags.Adjective, EnglishWordTags.Comparative),
    superlative -> List(EnglishWordTags.Adjective, EnglishWordTags.Superlative),
  )
}
