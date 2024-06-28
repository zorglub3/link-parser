package link.writer

import link.language._
import link.language.VerbPhrase.Tense

trait LanguageWriter[N, W, WB <: WriteBuffer[W, WB]] {
  import LanguageWriter._
  
  type Result = WB => Either[WriteError[W], WB]

  val writeNothing = { x: WB => Right(x) }

  def write(w: W): Result = { x: WB => Right(x.addWord(w)) }

  def writeE(w: Either[WriteError[W], W]): Result = { x: WB => 
    w.flatMap(y => Right(x.addWord(y)))
  }

  implicit class ResultSyntax(r: Result) {
    def seq(r2: => Result): Result = { x => r(x).flatMap(r2) }
  }

  def sequence(rs: List[Result]): Result = {
    rs match {
      case Nil => writeNothing
      case h :: t => h.seq(sequence(t))
    }
  }
  
  def writeSentence(sentence: SimpleSentence[N, W]): Result = {
    import SimpleSentence._
    
    sentence match {
      case Imperative(vp) => writeVP(None, vp)
      case Statement(np, vp) => writeNP(np, Casus.Nominative).seq(writeVP(npForm(np), vp))
      case Question(_, _, _) => ???
    }
  }

  def writeVP(form: Option[VerbForm], vp: VerbPhrase[N, W]): Result = {
    import VerbPhrase._
    
    vp match {
      case IntransitiveVerbPhrase(w, t, ps) => {
        val adverbs = ps.filter(_.adverbialLike)
        val prepositions = ps.filter(_.prepositionLike)

        writeAdverbs(adverbs) 
          .seq(writeVerb(form, w, t)) 
          .seq(writePrepositions(prepositions))
      }
      case TransitiveVerbPhrase(w, t, o, ps) => {
        val adverbs = ps.filter(_.adverbialLike)
        val prepositions = ps.filter(_.prepositionLike)

        writeAdverbs(adverbs)
          .seq(writeVerb(form, w, t))
          .seq(writeNP(o, Casus.Accusative))
          .seq(writePrepositions(prepositions))
      }
      case LinkVerbPhrase(w, t, p, ps) => {
        val adverbs = ps.filter(_.adverbialLike)
        val prepositions = ps.filter(_.prepositionLike)

        writeAdverbs(adverbs)
          .seq(writeVerb(form, w, t))
          .seq(writePredicate(p))
          .seq(writePrepositions(prepositions))
      }
      case HelpVerbPhrase(v, t, vp) => {
        // STUB
        ???
      }
    }
  }

  def writeAdverbs(adverbs: List[Predicate[N, W]]): Result = {
    import Predicate._
    
    sequence(adverbs.map {
      case Adverbial(p) => write(p)
      case _ => writeNothing
    } )    
  }

  def writePrepositions(prepositions: List[Predicate[N, W]]): Result = {
    import Predicate._

    sequence(prepositions.map {
      case PositionPredicate(p, np) => write(p).seq(writeNP(np, Casus.Accusative))
      case TransitiveVerbingPredicate(w, np) => {
        writeVerb(None, w, VerbPhrase.PresentParticiple)
          .seq(writeNP(np, Casus.Accusative))
      }
      case _ => writeNothing
    })
  }

  def writePredicate(predicate: Predicate[N, W]): Result = {
    import Predicate._

    predicate match {
      case SimplePredicate(r, s) => writeAdjective(r, s)
      case VerbingPredicate(r) => writeVerb(None, r, VerbPhrase.PresentParticiple)
      case PositionPredicate(p, np) => write(p).seq(writeNP(np, Casus.Accusative))
      case _ => writeNothing
    }
  }
  
  def npForm(np: N): Option[VerbForm]
  def writeVerb(form: Option[VerbForm], root: W, tense: Tense): Result
  def writeAdjective(root: W, superlative: Boolean): Result  
  def writeNP(np: N, casus: Casus): Result
}

object LanguageWriter {
  case class VerbForm(number: Int, plural: Boolean)
}
