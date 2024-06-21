package link.english

import link.language.NounPhrase.Gender
import link.writer.Casus
import link.writer.Writer.VerbForm
import link.language.VerbPhrase
import link.language.VerbPhrase.Tense
import link.english.lexicon.{EnglishWordTags => WT, EnglishLexiconEntry}
import link.english.lexicon.EnglishLexiconEntry.WordEntry

import scala.collection.immutable.HashMap

class WordBook(entries: List[EnglishLexiconEntry]) {
  import WordBook._
  
  private val nounMapBuilder = HashMap.newBuilder[NounLookup, String]
  private val verbMapBuilder = HashMap.newBuilder[VerbLookup, String]

  private def nounRoot(we: WordEntry): Option[String] =
    we.tags.collectFirst { case WT.NounRoot(root) => root }    

  private def verbRoot(we: WordEntry): Option[String] =
    we.tags.collectFirst { case WT.VerbRoot(root) => root }

  private def verbIsPlural(we: WordEntry): Boolean =
    we.tags.contains(WT.Plural)

  private def verbTense(we: WordEntry): Tense = {
    if(we.tags.contains(WT.Root)) {
      VerbPhrase.Imperative
    } else if(we.tags.contains(WT.Present)) {
      VerbPhrase.Present
    } else if(we.tags.contains(WT.Past)) {
      VerbPhrase.Past
    } else if(we.tags.contains(WT.PresentParticiple)) {
      VerbPhrase.PresentParticiple
    } else if(we.tags.contains(WT.PastParticiple)) {
      VerbPhrase.PastParticiple
    } else {
      VerbPhrase.Imperative
    }
  }
    
  for {
    e <- entries
    w <- e.wordEntries
  } {
    nounRoot(w) foreach { root =>
      nounMapBuilder += NounLookup(root, w.tags.contains(WT.Plural)) -> w.word
    }

    verbRoot(w) foreach { root =>
      if(root != "be") {
        verbMapBuilder += VerbLookup(root, verbIsPlural(w), verbTense(w)) -> w.word
      }
    }
  }

  val nouns = nounMapBuilder.result()
  val verbs = verbMapBuilder.result()
  
  def lookupNoun(root: String, plural: Boolean): String = {
    nouns.getOrElse(NounLookup(root, plural), s"[missing noun for $root]")
  }

  def lookupPronoun(person: Int, plural: Boolean, gender: Option[Gender], casus: Casus): String = ???
  def lookupAdjective(root: String, superlative: Boolean): String = ???

  def lookupVerb(root: String, verbForm: Option[VerbForm], tense: Tense): String = {
    if(root == "be") {
      (verbForm, tense) match {
        case (Some(VerbForm(1, false)), VerbPhrase.Present) => "am"
        case (Some(VerbForm(2, false)), VerbPhrase.Present) => "are"
        case (Some(VerbForm(3, false)), VerbPhrase.Present) => "is"
        case (_, VerbPhrase.Present) => "are"
        case (Some(VerbForm(1, false)), VerbPhrase.Past) => "was"
        case (Some(VerbForm(3, false)), VerbPhrase.Past) => "was"
        case (_, VerbPhrase.Past) => "were"
        case _ => "[... to be ...]"
      }      
    } else {
      verbForm match {
        case None => verbs.getOrElse(VerbLookup(root, false, tense), s"[missing verb for: $root]")
        case Some(VerbForm(_, plural)) => verbs.getOrElse(VerbLookup(root, plural, tense), s"[missing verbfor: $root]")
      }
    }  
  }
}

object WordBook {
  case class NounLookup(root: String, plural: Boolean)
  case class VerbLookup(root: String, plural: Boolean, tense: Tense)
}
