package link.parser

import link.rule._
import link.english.lexicon.EnglishLinkTags
import link.rule.LinkRuleSyntax._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkParserSpec extends AnyFlatSpec with Matchers {
  def linkRules() = new LinkRuleSet[String] {
    import EnglishLinkTags._

    "the" :- r(D)
    "dog" :- (l(D) & r(Ss))
    "dogs" :- (opt(l(D)) & r(Sp))
    "runs" :- l(Ss)
    "run"  :- l(Sp)
    "ran"  :- (l(Ss) | l(Sp))
    "////" :- opt(r(W))
  }

  "A link parser" should "check and accept simple sentences" in {
    val rules = linkRules()
    val parser = new LinkParser[String](rules.wordRules.toMap)

    parser.check(List("the", "dog", "runs")) shouldBe 1
    parser.check(List("dogs", "run")) shouldBe 1
    parser.check(List("the", "dogs", "run")) shouldBe 1
    parser.check(List("the", "dog", "ran")) shouldBe 1
    parser.check(List("the", "dogs", "ran")) shouldBe 1
  }

  it should "find correct number of sets of links in simple sentences" in {
    val rules = linkRules()
    val parser = new LinkParser[String](rules.wordRules.toMap)

    parser.links(List("the", "dog", "runs")).length shouldBe 1
    parser.links(List("dogs", "run")).length shouldBe 1
    parser.links(List("the", "dogs", "run")).length shouldBe 1
    parser.links(List("the", "dog", "ran")).length shouldBe 1
    parser.links(List("the", "dogs", "ran")).length shouldBe 1
  }

  it should "check a sentence even when there is a wall" in {
    val rules = linkRules()
    val parser = new LinkParser[String](rules.wordRules.toMap)

    // parser.check(List("////", "\\\\\\\\")) shouldBe 1
    parser.check(List("////", "the", "dog", "runs")) shouldBe 1
  }

  it should "parse a sentence even when there is a wall" in {
    val rules = linkRules()
    val parser = new LinkParser[String](rules.wordRules.toMap)

    // parser.links(List("////", "\\\\\\\\")).length shouldBe 1
    parser.links(List("////", "the", "dog", "runs")).length shouldBe 1
  }

  it should "not parse a noun phrase" in {
    val rules = linkRules()

    val parser = new LinkParser[String](rules.wordRules.toMap)

    parser.check(List("the", "dog")) shouldBe 0
    parser.links(List("the", "dog")).length shouldBe 0
  }

  it should "not parse a single plural noun" in {
    val rules = linkRules()

    val parser = new LinkParser[String](rules.wordRules.toMap)

    parser.check(List("dogs")) shouldBe 0
    parser.links(List("dogs")).length shouldBe 0
  }

  it should "not parse malformed sentences" in {
    val rules = linkRules()

    val parser = new LinkParser[String](rules.wordRules.toMap)

    parser.check(List("the", "runs", "dog")) shouldBe 0
    parser.links(List("the", "runs", "dog")).length shouldBe 0
  }
}
