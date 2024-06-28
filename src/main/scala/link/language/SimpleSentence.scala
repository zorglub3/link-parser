package link.language

import ContextMapper.UnmappedObject

sealed abstract class SimpleSentence[N, W](val vp: VerbPhrase[N, W]) { Self =>
  val subject: Option[N]

  def mapNP[M, CM <: ContextMapper[N, M, CM]](
    context: CM
  ): Either[UnmappedObject[N], (SimpleSentence[M, W], CM)]
}

object SimpleSentence {
  final case class Imperative[N, W](_vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = None

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (SimpleSentence[M, W], CM)] = {
      for {
        p <- vp.mapNP[M, CM](context)
      } yield (Imperative(p._1), p._2)
    }
  }

  final case class Statement[N, W](np: N, _vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = Some(np)

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (SimpleSentence[M, W], CM)] = {
      for {
        p2 <- context.mapNP(np)
        p3 <- vp.mapNP[M, CM](p2._2)
      } yield (Statement(p2._1, p3._1), p3._2)
    }
  }

  final case class Question[N, W](mode: QuestionMode, np: N, _vp: VerbPhrase[N, W]) extends SimpleSentence[N, W](_vp) {
    val subject = Some(np)

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (SimpleSentence[M, W], CM)] = {
      for {
        p2 <- context.mapNP(np)
        p3 <- vp.mapNP[M, CM](p2._2)
      } yield (Question(mode, p2._1, p3._1), p3._2)
    }
  }
}
