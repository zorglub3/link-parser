package link.language

sealed abstract class SimpleSentence[N, W](val vp: VerbPhrase[N, W]) { Self =>
  val subject: Option[N]

  def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (SimpleSentence[M, W], ContextMapper[N, M])]
}

object SimpleSentence {
  final case class Imperative[N, W](_vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = None

    def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (SimpleSentence[M, W], ContextMapper[N, M])] = {
      for {
        p <- vp.mapNP(context)
        (vp2, context2) = p
      } yield (Imperative(vp2), context2)
    }
  }

  final case class Statement[N, W](np: N, _vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = Some(np)
    def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (SimpleSentence[M, W], ContextMapper[N, M])] = {
      for {
        p2 <- context.mapNP(np)
        (np2, context2) = p2
        p3 <- vp.mapNP(context2)
        (vp3, context3) = p3
      } yield (Statement(np2, vp3), context3)
    }
  }

  // TODO predicate
  final case class LinkStatement[N, W](np: N, link: W) 
  extends SimpleSentence[N, W](VerbPhrase.LinkVerbPhrase[N, W](link)) {
    val subject = Some(np)
    def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (SimpleSentence[M, W], ContextMapper[N, M])] = {
      for {
        p2 <- context.mapNP(np)
        (np2, context2) = p2
        // TODO predicate
      } yield (LinkStatement(np2, link), context2)
    }
  }
}
