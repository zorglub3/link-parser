package link.language

import ContextMapper.UnmappedObject

sealed abstract class VerbPhrase[N, W](val verb: W, tense: Tense) {
  val obj: Option[N]

  def mapNP[M, CM <: ContextMapper[N, M, CM]](
    context: CM
  ): Either[UnmappedObject[N], (VerbPhrase[M, W], CM)]
}

object VerbPhrase {
  final case class IntransitiveVerbPhrase[N, W](
    v: W, t: Tense, 
    predicates: List[Predicate[N, W]]
  ) extends VerbPhrase[N, W](v, t) {
    val obj = None

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (VerbPhrase[M, W], CM)] = {
      for {
        p <- Predicate.mapPredicates[W, N, M, CM](context, predicates)
      } yield (IntransitiveVerbPhrase(v, t, p._1), p._2)
    }
  }

  final case class TransitiveVerbPhrase[N, W](v: W, t: Tense, o: N, predicates: List[Predicate[N, W]]) 
  extends VerbPhrase[N, W](v, t) {
    val obj = Some(o)

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (VerbPhrase[M, W], CM)] = {
      for {
        p1 <- context.mapNP(o)
        p2 <- Predicate.mapPredicates[W, N, M, CM](p1._2, predicates)
      } yield (TransitiveVerbPhrase(v, t, p1._1, p2._1), p2._2)
    }
  }

  final case class LinkVerbPhrase[N, W](v: W, t: Tense, p: Predicate[N, W], predicates: List[Predicate[N, W]]) 
  extends VerbPhrase[N, W](v, t) {
    val obj = None

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (VerbPhrase[M, W], CM)] = {
      for {
        p1 <- p.mapNP[M, CM](context)
        p2 <- Predicate.mapPredicates[W, N, M, CM](p1._2, predicates)
      } yield (LinkVerbPhrase(v, t, p1._1, p2._1), p2._2)
    }
  }

  final case class HelpVerbPhrase[N, W](v: W, t: Tense, vp: VerbPhrase[N, W]) 
  extends VerbPhrase[N, W](vp.verb, t) {
    val obj = None

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (VerbPhrase[M, W], CM)] = {
      for {
        p <- vp.mapNP[M, CM](context)
      } yield (HelpVerbPhrase(v, t, p._1), p._2)
    }
  }
}
