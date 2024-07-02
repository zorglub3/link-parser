package link.language

import ContextMapper.UnmappedObject

sealed abstract class Predicate[N, W] {
  def adjectiveLike: Boolean = false
  def prepositionLike: Boolean = false
  def adverbialLike: Boolean = false
  def linkObjectLike: Boolean = false

  def mapNP[M, CM <: ContextMapper[N, M, CM]](
    context: CM
  ): Either[UnmappedObject[N], (Predicate[M, W], CM)]
}

object Predicate {
  case class SimplePredicate[N, W](root: W, superlative: Boolean) extends Predicate[N, W] {
    override def adjectiveLike = true
    override def linkObjectLike = true

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (Predicate[M, W], CM)] = {
      Right( (SimplePredicate(root, superlative), context) ) 
    }
  }

  case class Comparison[N, W](root: W, other: N) extends Predicate[N, W] {
    override def linkObjectLike = true

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (Predicate[M, W], CM)] = {
      for {
        p <- context.mapNP(other)
      } yield (Comparison(root, p._1), p._2)
    }
  }

  // TODO this phrase can have prepositions (and also adverbials)
  case class VerbingPredicate[N, W](root: W) extends Predicate[N, W] {
    // TODO if this phrase has prepositions it will be _prepositionLike_ (not adverbial-like)
    override def adjectiveLike = true
    override def linkObjectLike = true

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (Predicate[M, W], CM)] = {
      Right( (VerbingPredicate(root), context) )
    }
  }

  // TODO can also have prepositions (like VerbingPhrase).
  case class TransitiveVerbingPredicate[N, W](root: W, obj: N) extends Predicate[N, W] {
    override def prepositionLike = true
    override def linkObjectLike = true

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (Predicate[M, W], CM)] = {
      for {
        p <- context.mapNP(obj)
      } yield ( (TransitiveVerbingPredicate(root, p._1), p._2) )
    } 
  }

  case class PositionPredicate[N, W](p: W, np: N) extends Predicate[N, W] {
    override def prepositionLike = true
    override def linkObjectLike = true

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (Predicate[M, W], CM)] = {
      for {
        pair <- context.mapNP(np)
      } yield ( (PositionPredicate(p, pair._1), pair._2) )
    }
  }

  case class Adverbial[N, W](p: W) extends Predicate[N, W] {
    override def adverbialLike = true

    def mapNP[M, CM <: ContextMapper[N, M, CM]](
      context: CM
    ): Either[UnmappedObject[N], (Predicate[M, W], CM)] = {
      Right( (Adverbial(p), context) )
    }
  }
}
