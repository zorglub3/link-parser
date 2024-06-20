package link.english.lexicon

import link.rule.LinkRuleSyntax
import link.english.lexicon.EnglishLinkTags._

// TODO improve adverb and preposition handling (prepositions don't work here)

case class LinkVerb(
  root: String,
  presentSingular: String,
  presentPlural: String,
  presentParticiple: String,
  past: String,
  pastParticiple: String,
) extends EnglishLexiconEntry {
  import LinkRuleSyntax._
  import EnglishLexiconEntry.WordEntry
  import EnglishWordTags._

  val activeVerbWithPredicate =
    opt(l(A)) & (r(P) | r(B))

  val wordEntries =
    List(
      WordEntry(
        root,
        List(Verb, EnglishWordTags.LinkVerb, Root, VerbRoot(root)),
        ((opt(l(Hr)) & l(W) & activeVerbWithPredicate) | 
         (l(Ss) & l(Hs) & activeVerbWithPredicate) | 
         (l(Sp) & l(Hp) & activeVerbWithPredicate) | 
         (l(Hp) & l(Sp) & activeVerbWithPredicate) | 
         (l(Hs) & l(Ss) & activeVerbWithPredicate)), 
      ),
      WordEntry(
        presentSingular,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), Present),
        (opt(l(A)) & l(Ss) & activeVerbWithPredicate),
      ),
      WordEntry(
        presentPlural,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), Present),
        (opt(l(A)) & l(Sp) & activeVerbWithPredicate),
      ),
      WordEntry(
        past,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), Past),
        (opt(l(A)) & l(S) & activeVerbWithPredicate),
      ),
      WordEntry(
        presentParticiple,
        List(Verb, EnglishWordTags.LinkVerb, VerbRoot(root), PresentParticiple),
        (l(Tr) & r(O)),
      ),
      // TODO past participle
    )
}
