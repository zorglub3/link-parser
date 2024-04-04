package link.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SentenceGraphSpec extends AnyFlatSpec with Matchers {
  "A sentence edge" should "remain unchanged in apply/unapply roundtrip" in {
    import SentenceEdgeSyntax._
    import link.english.lexicon.EnglishLinkTags._
        
    val e = 1 ~ 2 :+ S

    e match {
      case 1 :~ 2 +: S => { /* success */ }
      case e => { fail(s"Roundtrip mismatch $e")}
    }
  }

  it should "remain unchanged for unapply from graph roundtrip" in {
    import SentenceEdgeSyntax._
    import link.english.lexicon.EnglishLinkTags._

    val e = 1 ~ 2 :+ S
    val g = ImmutableSentenceGraph.from(List(1, 2), List(e))

    g.edges.toList.length shouldBe 1

    g.edges.map(_.outer).collect {
      case 1 :~ 2 +: S => { /* success */ }
      case e => fail(s"Unexpected edge $e")
    }
  }
}
