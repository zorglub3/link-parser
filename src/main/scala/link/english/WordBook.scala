package link.english

import link.language.NounPhrase.Gender
import link.writer.Casus
import link.writer.LanguageWriter.VerbForm
import link.writer.WriteError
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
  
  def lookupNoun(root: String, plural: Boolean): Either[WriteError[String],String] = {
    nouns
      .get(NounLookup(root, plural))
      .map(Right(_))
      .getOrElse(Left(WriteError(root, "noun")))
  }

  def lookupPronoun(person: Int, plural: Boolean, gender: Option[Gender], casus: Casus): Either[WriteError[String], String] = ???
  def lookupAdjective(root: String, superlative: Boolean): String = ???

  def lookupVerb(root: String, verbForm: Option[VerbForm], tense: Tense): Either[WriteError[String], String] = {
    if(root == "be") {
      (verbForm, tense) match {
        case (Some(VerbForm(1, false)), VerbPhrase.Present) => Right("am")
        case (Some(VerbForm(2, false)), VerbPhrase.Present) => Right("are")
        case (Some(VerbForm(3, false)), VerbPhrase.Present) => Right("is")
        case (_, VerbPhrase.Present) => Right("are")
        case (Some(VerbForm(1, false)), VerbPhrase.Past) => Right("was")
        case (Some(VerbForm(3, false)), VerbPhrase.Past) => Right("was")
        case (_, VerbPhrase.Past) => Right("were")
        case _ => Right("[... to be ...]")
      }      
    } else {
      verbForm match {
        case None => {
          verbs
            .get(VerbLookup(root, false, tense))
            .map(Right(_))
            .getOrElse(Left(WriteError(root, "verb")))
        }
        case Some(VerbForm(_, plural)) => {
          verbs
            .get(VerbLookup(root, plural, tense))
            .map(Right(_))
            .getOrElse(Left(WriteError(root, "verb")))
        }
      }
    }  
  }
}

object WordBook {
  case class NounLookup(root: String, plural: Boolean)
  case class VerbLookup(root: String, plural: Boolean, tense: Tense)
  case class PronounLookup(person: Int, plural: Boolean, gender: Option[Gender], casus: Casus)
}
