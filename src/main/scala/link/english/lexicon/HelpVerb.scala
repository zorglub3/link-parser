package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class HelpVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags.{Verb, Root, VerbRoot, Present, Past, Singular, Plural}

  val wordEntries = 
    List(
      WordEntry(
        root,
        List(EnglishWordTags.HelpVerb, Verb, Root, VerbRoot(root)),
        opt(r(N)) & r(Hr),
      ), 
      WordEntry(
        presentSingular,
        List(EnglishWordTags.HelpVerb, Verb, Present, Singular, VerbRoot(root)),
        opt(l(Q)) & opt(r(N)) & r(Hs),
      ),
      WordEntry(
        presentPlural,
        List(EnglishWordTags.HelpVerb, Verb, Present, Plural, VerbRoot(root)),
        opt(l(Q)) & opt(r(N)) & r(Hp),
      ),
      WordEntry(
        past,
        List(EnglishWordTags.HelpVerb, Verb, Past, VerbRoot(root)),
        opt(l(Q)) & opt(r(N)) & r(H),
      )
    )
}
