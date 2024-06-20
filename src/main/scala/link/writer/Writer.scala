package link.writer

import link.language._

trait Writer[N, W] {
  import Writer._
  
  def writeSentence(s: SimpleSentence[N, W]): Unit = {
    s match {
      case SimpleSentence.Imperative(vp) => {
        writeVP(None, vp)
      }
      case SimpleSentence.Statement(np, vp) => {
        val form = npForm(np)
        writeNP(np)
        writeVP(Some(form), vp)
      }
      case SimpleSentence.Question(_, _, _) => ???
    }
  }

  def writeVP(verbForm: Option[VerbForm], vp: VerbPhrase[N, W]): Unit = {
    vp match {
      case VerbPhrase.IntransitiveVerbPhrase(w, t, ps) => {
        // TODO write adverbs
        writeVerb(verbForm, w, t)
        // TODO write prepositions
      }
      case VerbPhrase.TransitiveVerbPhrase(w, t, o, ps) => {
        // TODO write adverbs
        writeVerb(verbForm, w, t)
        writeNP(o)
        // TODO write prepositions
      }
      case VerbPhrase.LinkVerbPhrase(w, t, p, ps) => {
        // TODO write adverbs
        writeVerb(verbForm, w, t)
        // TODO write predicate
        // TODO write prepositions
      }
      case VerbPhrase.HelpVerbPhrase(v, t, vp) => {
        ???
      }
    }    
  }

  def npForm(np: N): VerbForm
  def writeNP(np: N): Unit
  def writeWord(w: W): Unit
  def writeVerb(verbForm: Option[VerbForm], w: W, tense: VerbPhrase.Tense): Unit
}

object Writer {
  case class VerbForm(number: Int, plural: Boolean)
}
