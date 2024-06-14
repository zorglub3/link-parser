package demo

import link.tokenizer.Tokenizer
import link.parser.LinkParser
import link.english._
import link.english.interpreter.EnglishInterpreter

class Demo {
  val builder = 
    new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns with StandardAdjectives with StandardPrepositions

  val tokenLexicon = builder.tokenLexicon
  val tokenizer = new Tokenizer[String](tokenLexicon, " ")
  val parser = new LinkParser[String](builder.ruleMap)

  def parseLinks(str: String) = {
    tokenizer(str.toLowerCase).map { tokens => parser.links(tokens) }
  }

  def interpret(str: String) = {
    tokenizer(str.toLowerCase).map { tokens =>
      val results = parser.links(tokens)
      results.map { r =>
        val int = new EnglishInterpreter(r)
        int.interpretS()
      }
    }
  }
}
