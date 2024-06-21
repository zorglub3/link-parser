package demo

import link.LinkError
import link.tokenizer.Tokenizer
import link.parser.LinkParser
import link.english._
import link.english.interpreter.EnglishInterpreter
import link.english.writer.EnglishWriter

class Demo {
  val builder = 
    new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns with StandardAdjectives with StandardPrepositions with StandardAdverbs

  val tokenLexicon = builder.tokenLexicon
  val wordBook = builder.wordBook
  val tokenizer = new Tokenizer[String](tokenLexicon, " ")
  val parser = new LinkParser[String](builder.ruleMap)
  val writer = new EnglishWriter(wordBook)

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

  def roundTrip(str: String): Either[LinkError, List[String]] = {
    for {
      tokens <- tokenizer(str.toLowerCase)
      results = parser.links(tokens)
      ints = results.flatMap(EnglishInterpreter.apply _)
      out = ints.map(writer.writeOne _)
    } yield out
  }
}
