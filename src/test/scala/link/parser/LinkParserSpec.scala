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

  def ruleMap(): RuleMap[String] = linkRules().makeRuleMap
  
  "A link parser" should "check and accept simple sentences" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("the", "dog", "runs")) shouldBe 1
    parser.check(Vector("dogs", "run")) shouldBe 1
    parser.check(Vector("the", "dogs", "run")) shouldBe 1
    parser.check(Vector("the", "dog", "ran")) shouldBe 1
    parser.check(Vector("the", "dogs", "ran")) shouldBe 1
  }

  it should "find correct number of sets of links in simple sentences" in {
    val parser = new LinkParser[String](ruleMap())

    parser.links(Vector("the", "dog", "runs")).length shouldBe 1
    parser.links(Vector("dogs", "run")).length shouldBe 1
    parser.links(Vector("the", "dogs", "run")).length shouldBe 1
    parser.links(Vector("the", "dog", "ran")).length shouldBe 1
    parser.links(Vector("the", "dogs", "ran")).length shouldBe 1
  }

  it should "check a sentence even when there is a wall" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("////", "the", "dog", "runs")) shouldBe 1
  }

  it should "parse a sentence even when there is a wall" in {
    val parser = new LinkParser[String](ruleMap())

    parser.links(Vector("////", "the", "dog", "runs")).length shouldBe 1
  }

  it should "not parse a noun phrase" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("the", "dog")) shouldBe 0
    parser.links(Vector("the", "dog")).length shouldBe 0
  }

  it should "not parse a single plural noun" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("dogs")) shouldBe 0
    parser.links(Vector("dogs")).length shouldBe 0
  }

  it should "not parse malformed sentences" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("the", "runs", "dog")) shouldBe 0
    parser.links(Vector("the", "runs", "dog")).length shouldBe 0
  }
}
