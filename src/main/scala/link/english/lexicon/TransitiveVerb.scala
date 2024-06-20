package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class TransitiveVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val activeVerbWithObject = opt(l(A)) & r(O) & opt(r(A)) & opt(r(P))

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Verb, Root, Transitive),
        ((opt(l(Hr)) & l(W) & activeVerbWithObject) | 
         (opt(l(N)) & l(Ss) & l(Hs) & activeVerbWithObject) | 
         (opt(l(N)) & l(Sp) & l(Hp) & activeVerbWithObject) | 
         (l(Hp) & l(Sp) & activeVerbWithObject) | 
         (l(Hs) & l(Ss) & activeVerbWithObject)), // TODO - opt(l("Hr")) & l("W") ...
      ),
      WordEntry(
        presentSingular,
        List(Verb, VerbRoot(root), Singular, Transitive, Present),
        (opt(l(A)) & l(Ss) & activeVerbWithObject),
      ),
      WordEntry(
        presentPlural,
        List(Verb, VerbRoot(root), Plural, Transitive, Present),
        (opt(l(A)) & l(Sp) & activeVerbWithObject),
      ),
      WordEntry(
        past,
        List(Verb, VerbRoot(root), Past, Transitive),
        (opt(l(A)) & l(S) & activeVerbWithObject),
      ),
      // TODO - present participle can also be used as adjectives
      WordEntry(
        presentParticiple,
        List(Verb, VerbRoot(root), PresentParticiple, Transitive),
        (l(Tr) & r(O)),
      ),
      // TODO past participle
    )
}
