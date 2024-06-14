package link.language

sealed abstract class Predicate[N, W]

sealed trait VerbPredicate[N, W] { self: Predicate[N, W] => }
sealed trait NounPredicate[N, W] { self: Predicate[N, W] => }

object Predicate {
  case class SimplePredicate[N, W](root: W, superlative: Boolean) extends Predicate[N, W] with NounPredicate[N, W]
  case class PositionPredicate[N, W](p: W, np: N) extends Predicate[N, W] with NounPredicate[N, W] with VerbPredicate[N, W]
  case class Adverbial[N, W](p: W) extends Predicate[N, W] with VerbPredicate[N, W]
}
