package link.language

abstract class VerbPhrase[N, W](val verb: W) {
  val obj: Option[N]
}

case class IntransitiveVerbPhrase[N, W](v: W) extends VerbPhrase[N, W](v) {
  val obj = None
}

case class TransitiveVerbPhrase[N, W](v: W, o: N) extends VerbPhrase[N, W](v) {
  val obj = Some(o)
}
