package demo

import link.tokenizer.Tokenizer
import link.parser.LinkParser
import link.english._

object Demo {
  def b = new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns
  def tokenLexicon = b.tokenLexicon
  def tokenizer = new Tokenizer[String](tokenLexicon, " ")
  def rules = b.linkRules
  def parser = new LinkParser[String](rules)
}
