package link.language

sealed abstract class VerbPhrase[N, W](val verb: W) {
  val obj: Option[N]

  def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (VerbPhrase[M, W], ContextMapper[N, M])] = ???
}

object VerbPhrase {
  final case class IntransitiveVerbPhrase[N, W](v: W) extends VerbPhrase[N, W](v) {
    val obj = None
  }

  final case class TransitiveVerbPhrase[N, W](v: W, o: N) extends VerbPhrase[N, W](v) {
    val obj = Some(o)
  }

  final case class LinkVerbPhrase[N, W](v: W) extends VerbPhrase[N, W](v) {
    val obj = None
  }
}
