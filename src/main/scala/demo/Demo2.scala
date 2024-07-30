package demo

import link.LinkError
import link.tokenizer.Tokenizer
import link.parser.LinkParser
import link.english._
import semantics.english.EnglishInterpreter
import amr._
// import link.english.writer.EnglishWriter

class Demo2 {
  val builder = 
    new EnglishLexiconBuilder 
      with StandardVerbs 
      with StandardWords 
      with StandardNouns 
      with StandardAdjectives 
      with StandardPrepositions 
      with StandardAdverbs

  val tokenLexicon = builder.tokenLexicon
  val wordBook = builder.wordBook
  val tokenizer = new Tokenizer[String](tokenLexicon, " ")
  val parser = new LinkParser[String](builder.ruleMap)
  val interpreter = new EnglishInterpreter
  // val writer = new EnglishWriter(wordBook)

  implicit class LinkErrorSyntax[E <: link.LinkError, T](v: Either[E, T]) {
    def coerce: Either[link.LinkError, T] = v
  }

  def parseLinks(str: String) = {
    for {
      tokens <- tokenizer(str.toLowerCase).coerce
      results <- parser.links(tokens).coerce
    } yield results
  }

  def interpret(str: String) = {
    for {
      tokens <- tokenizer(str.toLowerCase).coerce
      results <- parser.links(tokens).coerce
      semantics <- interpreter.interpretList(results).coerce
    } yield semantics
  }

  def ppAMR[L](r: Either[LinkError, List[AMR[String, L, Nothing]]]): Unit = {
    r match {
      case Left(err) => println(s"error: $err")
      case Right(l) => l.zipWithIndex.foreach { case (amr, index) => println(s"solution: $index:\n${amr.pp}")}
    }
  }

  /*
  TODO rewrite round trip
  def roundTrip(str: String): Either[LinkError, List[String]] = {
    for {
      tokens <- tokenizer(str.toLowerCase)
      results = parser.links(tokens)
      ints = results.flatMap(EnglishInterpreter.apply _)
      out = ints.map(writer.writeOne _)
    } yield out
  }
  */
}
