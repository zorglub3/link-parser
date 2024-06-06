package link.language

abstract class SimpleSentence[N, W](val vp: VerbPhrase[N, W]) { Self =>
  val subject: Option[N]

  def mapNp[M](f: N => M): SimpleSentence[M, W] = ??? // TODO - make stateful. Needs to keep track of context
}

object SimpleSentence {
  case class Imperative[N, W](_vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = None
  }

  case class Statement[N, W](np: N, _vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = Some(np)
  }

  case class LinkStatement[N, W](np: N, link: W) 
  extends SimpleSentence[N, W](VerbPhrase.LinkVerbPhrase[N, W](link)) {
    val subject = Some(np)
  }
}
