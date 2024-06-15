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

      t(s).map(p.links) match {
        case Right(xs) => { 
          xs.length should be >= 1
          
          xs.foreach { result =>
            val interpreter = new EnglishInterpreter(result)

            interpreter.interpretS() match {
              case Some(SimpleSentence.Imperative(_)) => {}
              case Some(x) => fail(s"Should be imperative sentence, got $x")
              case None => fail(s"Failed to interpret parsed sentence")
            }
          } 
        }
        case Left(e) => fail(s"Parsing produced error: $e")
      }
    }
  }

  for(s <- statementSentences) {
    it should s"interpret $s as a statement" in {
      val t = tokenizer
      val p = parser

      t(s).map(p.links) match {
        case Right(xs) => { 
          xs.length should be >= 1
          
          xs.foreach { result =>
            val interpreter = new EnglishInterpreter(result)

            interpreter.interpretS() match {
              case Some(SimpleSentence.Statement(_, _)) => {}
              case Some(x) => fail(s"Should be statement sentence, got $x")
              case None => fail(s"Failed to interpret parsed sentence")
            }
          } 
        }
        case Left(e) => fail(s"Parsing produced error: $e")
      }
    }
  }
}
