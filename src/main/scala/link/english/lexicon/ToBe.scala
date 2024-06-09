package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

case class ToBe() extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val toBeObj = r(B) | r(O) | r(P) | r(Tr)

  // TODO word tags
  val wordEntries =
    List(
      WordEntry(
        "am",
        List(),
        ((l(Spi) & opt(r(N)) & toBeObj)  | (r(Sq("pi")) & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "are",
        List(),
        ((l(Spp) & opt(r(N)) & toBeObj)  | (r(Sq("pp")) & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "is",
        List(),
        ((l(Ss) & opt(r(N)) & toBeObj)    | (r(Sq("s"))  & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "was",
        List(),
        ((l(Spi) & opt(r(N)) & toBeObj)  | (r(Sq("pi")) & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "was",
        List(),
        ((l(Ss) & opt(r(N)) & toBeObj)   | (r(Sq("s"))  & opt(r(N)) & toBeObj)),
      ),
      WordEntry(
        "were",
        List(),
        ((l(Spp) & opt(r(N)) & toBeObj) | (r(Sq("pp")) & opt(r(N)) & toBeObj)),
      ),
    )
}


