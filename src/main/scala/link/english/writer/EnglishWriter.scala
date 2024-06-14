package link.english.writer

import link.writer._
import link.language._
import scala.collection.mutable.StringBuilder

class EnglishWriter extends Writer[NounPhrase[String], String] {
  import Writer._
  
  val builder = new StringBuilder
  
  def npForm(np: NounPhrase[String]): VerbForm = {
    ???
  }

  def writeNP(np: NounPhrase[String]): Unit = {
    ???
  }

  def writeWord(w: String): Unit = {
    ???    
  }

  def writeVerb(verbForm: Option[VerbForm], w: String, tense: VerbPhrase.Tense): Unit = {
    ???
  }
}
