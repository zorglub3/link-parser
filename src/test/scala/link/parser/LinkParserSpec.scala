package link.parser

import link.rule._
import link.english.lexicon.EnglishLinkTags
import link.rule.LinkRuleSyntax._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class LinkParserSpec extends AnyFlatSpec with Matchers with EitherValues {
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

    parser.check(Vector("////", "the", "dog", "runs")) shouldBe 1
    parser.check(Vector("////", "dogs", "run")) shouldBe 1
    parser.check(Vector("////", "the", "dogs", "run")) shouldBe 1
    parser.check(Vector("////", "the", "dog", "ran")) shouldBe 1
    parser.check(Vector("////", "the", "dogs", "ran")) shouldBe 1
  }

  it should "find correct number of sets of links in simple sentences" in {
    val parser = new LinkParser[String](ruleMap())

    parser.links(Vector("////", "the", "dog", "runs")).map(_.length) shouldBe Right(1)
    parser.links(Vector("////", "dogs", "run")).map(_.length) shouldBe Right(1)
    parser.links(Vector("////", "the", "dogs", "run")).map(_.length) shouldBe Right(1)
    parser.links(Vector("////", "the", "dog", "ran")).map(_.length) shouldBe Right(1)
    parser.links(Vector("////", "the", "dogs", "ran")).map(_.length) shouldBe Right(1)
  }

  it should "check a sentence even when there is a wall" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("////", "the", "dog", "runs")) shouldBe 1
  }

  it should "parse a sentence even when there is a wall" in {
    val parser = new LinkParser[String](ruleMap())

    parser.links(Vector("////", "the", "dog", "runs")).map(_.length) shouldBe Right(1)
  }

  it should "not parse a noun phrase" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("////", "the", "dog")) shouldBe 0
    parser.links(Vector("////", "the", "dog")).isLeft shouldBe true
  }

  it should "not parse a single plural noun" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("////", "dogs")) shouldBe 0
    parser.links(Vector("////", "dogs")).isLeft shouldBe true
  }

  it should "not parse malformed sentences" in {
    val parser = new LinkParser[String](ruleMap())

    parser.check(Vector("////", "the", "runs", "dog")) shouldBe 0
    parser.links(Vector("////", "the", "runs", "dog")).isLeft shouldBe true 
  }

  it should "only use word tags from rules that are applicable" in {
    import link.english._
    import link.tokenizer.Tokenizer
    
    val b = 
      new EnglishLexiconBuilder 
        with StandardVerbs 
        with StandardWords 
        with StandardNouns 
        with StandardAdjectives 
        with StandardAdverbs
    val tokenLexicon = b.tokenLexicon
    val tokenizer = new Tokenizer[String](tokenLexicon, " ")
    val parser = new LinkParser[String](b.ruleMap)

    tokenizer("run").flatMap(parser.links) match {
      case Right(res1) => {
        import lexicon.EnglishWordTags._
        res1.head.tags(1) should contain theSameElementsAs List(Intransitive, Verb, Root, VerbRoot("run"))
      }
      case Left(_) => fail("sentence should parse")
    }
  }
}
