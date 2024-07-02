package link.english

import link.tokenizer.Tokenizer
import link.parser.LinkParser
import link.english.interpreter.EnglishInterpreter
import link.language._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnglishInterpreterSpec extends AnyFlatSpec with Matchers {
  def b = new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns with StandardAdjectives with StandardAdverbs
  def tokenLexicon = b.tokenLexicon
  def tokenizer = new Tokenizer[String](tokenLexicon, " ")
  def parser = new LinkParser[String](b.ruleMap)
  def interpreter = new EnglishInterpreter

  "An English parse result interpreter" should "dummy" in {
    true shouldBe true
  }
  
  val imperativeSentences =
    List(
      "pick up the table",
      "run over the road",
      // "quickly walk to the house",
      "look small",
    )

  val statementSentences =
    List(
      "the man walked to the house",
      "the men drink the drink",
      "you look small",
    )

  for(s <- imperativeSentences) {
    it should s"interpret $s as imperative" in {
      val t = tokenizer
      val p = parser
      val i = interpreter

      (for {
        tokens <- t(s)
        results <- p(tokens)
        semantics <- i(results)
      } yield {
        semantics.foreach { 
          case SimpleSentence.Imperative(_) => { /* ok */ }
          case x => fail(s"wrong interpretation. Got $x")
        } 

        semantics
      }).left.foreach { e => 
        fail(s"failed to parse/interpret sentence. Error: $e")
      }
    }
  }

  for(s <- statementSentences) {
    it should s"interpret $s as a statement" in {
      val t = tokenizer
      val p = parser
      val i = interpreter

      (for {
        tokens <- t(s)
        results <- p(tokens)
        semantics <- i(results)
      } yield {
        semantics.foreach { 
          case SimpleSentence.Statement(_, _) => { /* ok */ }
          case x => fail(s"wrong interpretation. Got $x")
        } 

        semantics
      }).left.foreach { e => 
        fail(s"failed to parse/interpret sentence. Error: $e")
      }
    }
  }
}
