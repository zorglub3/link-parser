package link.language

abstract class SimpleSentence[N, W](val vp: VerbPhrase[N, W]) {
  val subject: Option[N]
}

case class Imperative[N, W](_vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
  val subject = None
}

case class Statement[N, W](np: N, _vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
  val subject = Some(np)
}

