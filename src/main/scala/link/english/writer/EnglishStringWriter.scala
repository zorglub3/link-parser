package link.english.writer

import link.english.WordBook
import link.writer._
import link.language._
import link.language.VerbPhrase.Tense

class EnglishStringWriter[WB <: WriteBuffer[String, WB]](wordbook: WordBook) extends LanguageWriter[NounPhrase[String], String, WB] {
  import LanguageWriter.VerbForm
  
  def npForm(np: NounPhrase[String]): Option[VerbForm] = {
    np match {
      case NounPhrase.Pronoun(person, plural, _) => Some(VerbForm(person, plural))
      case NounPhrase.Thing(_, plural, _, _) => Some(VerbForm(3, plural))
    }
  }

  def writeVerb(form: Option[VerbForm], root: String, tense: Tense): Result = {
    writeE(wordbook.lookupVerb(root, form, tense))
  }

  def writeAdjective(root: String, superlative: Boolean): Result = ???

  def writeNoun(root: String, plural: Boolean): Result = 
    writeE(wordbook.lookupNoun(root, plural))

  def writeNP(np: NounPhrase[String], casus: Casus): Result = {
    import NounPhrase._
    
    np match {
      case Pronoun(person, plural, gender) => writeE(wordbook.lookupPronoun(person, plural, gender, casus))
      case Thing(determiner, plural, root, predicates) => {
        val adjectives = predicates.filter(_.adjectiveLike)
        val prepositions = predicates.filter(_.prepositionLike)

        write(determiner)
          .seq(sequence(adjectives.map(writePredicate)))
          .seq(writeNoun(root, plural))
          .seq(sequence(prepositions.map(writePredicate)))
      }
    }
  }
}
