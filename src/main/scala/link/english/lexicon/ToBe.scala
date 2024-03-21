package link.english.lexicon

import link.rule.{LinkRule, LinkRuleSyntax}
import link.english.lexicon.EnglishLinkTags._

case class ToBe() extends EnglishLexiconEntry {
  import LinkRuleSyntax._

  val toBeObj = r(B) | r(O) | r(P) | r(Tr)

  def linkRules: List[(String, LinkRule.NormalForm)] =
    List(
      "am"  -> ((l(Spi) & opt(r(N)) & toBeObj)  | (r(Sq("pi")) & opt(r(N)) & toBeObj)),
      "are" -> ((l(Spp) & opt(r(N)) & toBeObj)  | (r(Sq("pp")) & opt(r(N)) & toBeObj)),
      "is" -> ((l(Ss) & opt(r(N)) & toBeObj)    | (r(Sq("s"))  & opt(r(N)) & toBeObj)),
      "was" -> ((l(Spi) & opt(r(N)) & toBeObj)  | (r(Sq("pi")) & opt(r(N)) & toBeObj)),
      "was" -> ((l(Ss) & opt(r(N)) & toBeObj)   | (r(Sq("s"))  & opt(r(N)) & toBeObj)),
      "were" -> ((l(Spp) & opt(r(N)) & toBeObj) | (r(Sq("pp")) & opt(r(N)) & toBeObj)))

  def words = List("am", "are", "is", "was", "were")
}


