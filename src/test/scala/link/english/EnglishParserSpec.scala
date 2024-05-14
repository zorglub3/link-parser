package link.english

import link.tokenizer.Tokenizer
import link.parser.LinkParser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnglishParserSpec extends AnyFlatSpec with Matchers {
  def b = new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns with StandardAdjectives
  def tokenLexicon = b.tokenLexicon
  def tokenizer = new Tokenizer[String](tokenLexicon, " ")
  def rules = b.linkRules
  def parser = new LinkParser[String](rules)

  // Some tests are failing - not everything is finished here. It is TDD.
  // Failing tests are commented out. Uncomment while developing.
  // Note that the word "you" has two interpretations: singular and plural
  val sentences: List[(String, Int)] = List(
    "move" -> 1,
    "pick up the table" -> 1,
    "i move" -> 1,
    "he ran" -> 1,
    "it is a table" -> 1,
    "she is walking" -> 1,
    "you pick up the table" -> 2,
    "i pick up the table" -> 1,
    "she picked up the table" -> 1,
    "we are picking up the table" -> 1,
    "i am drinking" -> 1,
    "i was drinking" -> 1,
    "he run ran" -> 0,
    "he she it runs" -> 0,
    "you picks up the table" -> 0,
    "who picks up the table" -> 1,
    "who is picking up the table" -> 1,
    "who pick up the table" -> 0,
    "who runs" -> 1,
    "who run" -> 0,
    // "is it a table" -> 1,
    "the table is big" -> 1,
    "i am smallest" -> 1,
    "the dark table is biggest" -> 1,
    // "who are you" -> 2,
    "who is i" -> 0,
    // "who am i" -> 1,
    // "where is the table" -> 1,
    // "why are you picking up the table" -> 1,
    "i run over the table" -> 1,
    "i see the man with the telescope" -> 2,
    // "he does run" -> 1,
    // "he does not pick up the table" -> 1,
    // "he did not run" -> 1,
    // "who didn't run" -> 1,
    // "who doesn't run" -> 1,
    "it looks like a table" -> 1,
    "he looks small" -> 1,
    "look small" -> 1,
  )

  "An English parser checker" should "dummy" in {
    true shouldBe true
  }

  for(s <- sentences) {
    it should s"check ${s._2} parses for '${s._1}'" in {
      val t = tokenizer
      val p = parser

      t(s._1).map(p.check) shouldBe Right(s._2)
    }
  }

  "An english link parser" should "dummy" in {
    true shouldBe true
  }

  for(s <- sentences) {
    it should s"check '${s._1}' parses to ${s._2} sets of links" in {
      val t = tokenizer
      val p = parser

      t(s._1).map(p.links).map(_.length) shouldBe Right(s._2)
    }
  }

  it should "produce the correct graph for a sentence" in {
    val t = tokenizer
    val p = parser
    val s = "i run"

    val gs = t(s).map(p.links)

    import link.graph.SentenceEdgeSyntax._
    import link.english.lexicon.EnglishLinkTags._
    
    gs match {
      case Right(List(g)) => {
        g.edges.toList.length shouldBe 1
        assert(g.contains(1 ~ 2 :+ S))
      }
      case _ => fail("Should be a single graph")
    }
  }
}

