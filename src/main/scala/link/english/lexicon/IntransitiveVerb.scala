package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._
import link.english.lexicon.EnglishWordTags._

case class IntransitiveVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry

  val activeVerb = opt(l(A)) & opt(r(A)) & opt(r(P))

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Intransitive, Verb, Root, VerbRoot(root)),
        ((opt(l(Hr)) & l(W) & activeVerb) | 
         (l(Hs) & l(Ss) & activeVerb) | (l(Hp) & l(Sp) & activeVerb) | 
         (opt(l(N)) & l(Sp) & l(Hp) & activeVerb) | (opt(l(N)) & l(Ss) & l(Hs) & activeVerb)),
      ),
      WordEntry(
        presentSingular,
        List(Intransitive, Verb, Present, Singular, VerbRoot(root)),
        (opt(l(A)) & l(Ss) & activeVerb),
      ),
      WordEntry(
        presentPlural,
        List(Intransitive, Verb, Present, Plural, VerbRoot(root)),
        (opt(l(A)) & l(Sp) & activeVerb),
      ),
      WordEntry(
        past,
        List(Intransitive, Verb, Past, VerbRoot(root)),
        (opt(l(A)) & l(S) & activeVerb),
      ),
      WordEntry(
        presentParticiple,
        List(Intransitive, Verb, PresentParticiple, VerbRoot(root)),
        l(Tr),
      ),
      WordEntry(
        pastParticiple,
        List(Intransitive, Verb, PastParticiple, VerbRoot(root)),
        l(Ta), 
      )
    )
}
