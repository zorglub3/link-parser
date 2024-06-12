package link.language

abstract class Predicate[N, W]

object Predicate {
  case class SimplePredicate[N, W](p: W) extends Predicate[N, W]
  case class PositionPredicate[N, W](p: W, np: N) extends Predicate[N, W]
}
