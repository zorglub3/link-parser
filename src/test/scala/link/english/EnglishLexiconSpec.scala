package link.english

import link.tokenizer.{Tokenizer, UnrecognizedTokens}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnglishTokenizerSpec extends AnyFlatSpec with Matchers {
  def b = new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns
  def tokenLexicon = b.tokenLexicon
  def tokenizer = new Tokenizer[String](tokenLexicon, " ")
  def leftWall = Tokenizer.LEFT_WALL

  "An standard English tokenizer" should "recognize standard English words" in {
    val t = tokenizer

    t("who are you") shouldBe Right(List(leftWall, "who", "are", "you"))
  }

  it should "correctly concatenate tokens that have '_' in them" in {
    val t = tokenizer

    t("he picked up the table") shouldBe Right(List(leftWall, "he", "picked_up", "the", "table"))
  }

  it should "not recognize something non-English" in {
    val t = tokenizer

    t("hvem er du") shouldBe Left(UnrecognizedTokens(List("hvem", "er", "du")))
  }
}
