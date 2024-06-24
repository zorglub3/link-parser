package link.language

sealed abstract class Predicate[N, W] {
  def adjectiveLike: Boolean = false
  def prepositionLike: Boolean = false
  def adverbialLike: Boolean = false
  def linkObjectLike: Boolean = false

  def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (Predicate[M, W], ContextMapper[N, M])] = ???
}

sealed trait VerbPredicate[N, W] { self: Predicate[N, W] => }
sealed trait NounPredicate[N, W] { self: Predicate[N, W] => }

object Predicate {
  case class SimplePredicate[N, W](root: W, superlative: Boolean) extends Predicate[N, W] with NounPredicate[N, W] {
    override def adjectiveLike = true
    override def linkObjectLike = true
  }

  case class Comparison[N, W](root: W, other: N) extends Predicate[N, W] with NounPredicate[N, W] {
    override def linkObjectLike = true
  }

  case class VerbingPredicate[N, W](root: W) extends Predicate[N, W] with NounPredicate[N, W] {
    override def adjectiveLike = true
    override def linkObjectLike = true
  }

  case class TransitiveVerbingPredicate[N, W](root: W, obj: N) extends Predicate[N, W] with NounPredicate[N, W] {
    override def prepositionLike = true
  }

  case class PositionPredicate[N, W](p: W, np: N) extends Predicate[N, W] with NounPredicate[N, W] with VerbPredicate[N, W] {
    override def prepositionLike = true
    override def linkObjectLike = true
  }

  case class Adverbial[N, W](p: W) extends Predicate[N, W] with VerbPredicate[N, W] {
    override def adverbialLike = true
  }
}
