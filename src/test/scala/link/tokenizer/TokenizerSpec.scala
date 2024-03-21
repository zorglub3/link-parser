package link.tokenizer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerSpec extends AnyFlatSpec with Matchers {
  def builder = new StringTokenLexiconBuilder 

  val leftWall = Tokenizer.LEFT_WALL

  def tokenLexicon = {
    val b = builder

    b.addStringToken("the")
    b.addStringToken("fox")
    b.addStringToken("ran")
    
    b.result
  }

  def tokenizer = new Tokenizer[String](tokenLexicon, " ")

  "A tokenizer" should "tokenize a string into correct tokens" in {
    val t = tokenizer

    t("the fox ran") shouldBe Right(List(leftWall, "the", "fox", "ran"))
  }

  it should "give error for unrecognized tokens" in {
    val t = tokenizer

    t("the sheep runs") shouldBe Left(UnrecognizedTokens(List("sheep", "runs")))
  }

  it should "ignore whitespaces before after and in between tokens" in {
    val t = tokenizer

    t("  the   fox ran      ") shouldBe Right(List(leftWall, "the", "fox", "ran"))
  }
}
