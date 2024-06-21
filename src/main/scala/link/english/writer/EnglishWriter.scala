package link.english.writer

import link.english.WordBook
import link.writer._
import link.language._
import scala.collection.mutable.StringBuilder

class EnglishWriter(wordbook: WordBook) extends Writer[NounPhrase[String], String] {
  import Writer._
  
  val builder = new StringBuilder
  
  def npForm(np: NounPhrase[String]): VerbForm = {
    np match {
      case NounPhrase.Pronoun(person, plural, _) => VerbForm(person, plural)
      case NounPhrase.Thing(_, plural, _, _) => VerbForm(3, plural)
    }
  }

  def writePreposition(pp: NounPredicate[NounPhrase[String], String]): Unit = {
    pp match {
      case Predicate.PositionPredicate(p, np) => {
        writeWord(p)
        writeNP(np, Casus.Accusative)
      }
      case _ => {}
    }
  }

  def writeAdjective(a: NounPredicate[NounPhrase[String], String]): Unit = {
    a match {
      case Predicate.SimplePredicate(root, superlative) => {
        val word = wordbook.lookupAdjective(root, superlative)
        writeWord(word)
      }
      case Predicate.VerbingPredicate(root) => {
        val word = wordbook.lookupVerb(root, None, VerbPhrase.PresentParticiple)
        writeWord(word)
      }
      case _ => {}
    }    
  }

  def writeNP(np: NounPhrase[String], casus: Casus): Unit = {
    np match {
      case NounPhrase.Pronoun(person, plural, gender) => {
        val word = wordbook.lookupPronoun(person, plural, gender, casus)
        writeWord(word)
      }
      case NounPhrase.Thing(determiner, plural, thing, predicates) => {
        val adjectives = predicates.filter {
          case Predicate.SimplePredicate(_, _) => true
          case Predicate.VerbingPredicate(_) => true
          case _ => false
        }
        val prepositions = predicates.filter {
          case Predicate.PositionPredicate(_, _) => true
          case _ => false
        }

        val noun = wordbook.lookupNoun(thing, plural)
                
        writeWord(determiner)
        adjectives.foreach(writeAdjective)
        writeWord(noun)
        prepositions.foreach(writePreposition)
      }
    }
  }

  var makeSpace = false
  var capitalize = true
  
  def writeWord(w: String): Unit = {
    if(makeSpace) {
      builder.append(" ")      
    }

    if(capitalize) {
      builder.append(w.capitalize)
      capitalize = false
    } else {
      builder.append(w)
    }

    makeSpace = true
  }

  def writePunctuation(w: String): Unit = {
    builder.append(w)
    makeSpace = true

    if(w == ".") {
      capitalize = true
    }
  }

  def clear(): Unit = {
    builder.clear()
    makeSpace = false
    capitalize = true
  }

  def writeOne(s: SimpleSentence[NounPhrase[String], String]): String = {
    clear()
    writeSentence(s)
    builder.result()
  }

  def writeVerb(verbForm: Option[VerbForm], w: String, tense: VerbPhrase.Tense): Unit = {
    val word = wordbook.lookupVerb(w, verbForm, tense)
    writeWord(word)
  }
}
