package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class ToBe() extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val toBeObj = r(B) | r(O) | r(P) | r(Tr)

  // TODO - handle questions such as where/when/who is/are/am <obj>
  val wordEntries =
    List(
      WordEntry(
        "be",
        List(Verb, HelpVerb, VerbRoot("be"), Root),
        toBeObj,
      ),
      WordEntry(
        "am",
        List(Verb, HelpVerb, VerbRoot("be"), Present),
        ((l(Spi) & opt(r(N)) & toBeObj)  | (opt(l(Q)) & r(Sq("pi")) & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "are",
        List(Verb, HelpVerb, VerbRoot("be"), Present),
        ((l(Spp) & opt(r(N)) & toBeObj)  | (opt(l(Q)) & r(Sq("pp")) & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "is",
        List(Verb, HelpVerb, VerbRoot("be"), Present),
        ((l(Ss) & opt(r(N)) & toBeObj)    | (opt(l(Q)) & r(Sq("s"))  & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "was",
        List(Verb, HelpVerb, VerbRoot("be"), Past),
        ((l(Spi) & opt(r(N)) & toBeObj)  | (opt(l(Q)) & r(Sq("pi")) & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "was",
        List(Verb, HelpVerb, VerbRoot("be"), Past),
        ((l(Ss) & opt(r(N)) & toBeObj)   | (opt(l(Q)) & r(Sq("s"))  & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "were",
        List(Verb, HelpVerb, VerbRoot("be"), Past),
        ((l(Spp) & opt(r(N)) & toBeObj) | (opt(l(Q)) & r(Sq("pp")) & opt(r(N)) & toBeObj)),
      ),
    )
}
