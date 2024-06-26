package link.language

sealed abstract class VerbPhrase[N, W](val verb: W, tense: VerbPhrase.Tense) {
  val obj: Option[N]

  def adverbs: List[Predicate.Adverbial[N, W]]
  def prepositions: List[Predicate.PositionPredicate[N, W]]

  def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (VerbPhrase[M, W], ContextMapper[N, M])] = ???
}

sealed trait BaseVerbPhrase[N, W] { self: VerbPhrase[N, W] =>
  // def adverbs: List[Predicate.Adverbial[N, W]]
  // def prepositions: List[Predicate.PositionPredicate[N, W]]

  // def mapNP[M](context: ContextMapper[N, M]): Either[ContextMapper.UnmappedObject[N], (VerbPhrase[M, W], ContextMapper[N, M])] = ???
}

object VerbPhrase {
  final case class IntransitiveVerbPhrase[N, W](v: W, t: Tense, predicates: List[Predicate[N, W] with VerbPredicate[N, W]]) 
  extends VerbPhrase[N, W](v, t) with BaseVerbPhrase[N, W] {
    val obj = None

    def adverbs = predicates.collect { case x: Predicate.Adverbial[N, W] => x } .toList
    def prepositions = predicates.collect { case x: Predicate.PositionPredicate[N, W] => x } .toList
  }

  final case class TransitiveVerbPhrase[N, W](v: W, t: Tense, o: N, predicates: List[Predicate[N, W] with VerbPredicate[N, W]]) 
  extends VerbPhrase[N, W](v, t) with BaseVerbPhrase[N, W] {
    val obj = Some(o)
    def adverbs = predicates.collect { case x: Predicate.Adverbial[N, W] => x } .toList
    def prepositions = predicates.collect { case x: Predicate.PositionPredicate[N, W] => x } .toList
  }

  final case class LinkVerbPhrase[N, W](v: W, t: Tense, p: NounPredicate[N, W], predicates: List[Predicate[N, W] with VerbPredicate[N, W]]) 
  extends VerbPhrase[N, W](v, t) with BaseVerbPhrase[N, W] {
    val obj = None
    def adverbs = predicates.collect { case x: Predicate.Adverbial[N, W] => x } .toList
    def prepositions = predicates.collect { case x: Predicate.PositionPredicate[N, W] => x } .toList
  }

  final case class HelpVerbPhrase[N, W](v: W, t: Tense, vp: VerbPhrase[N, W] with BaseVerbPhrase[N, W]) 
  extends VerbPhrase[N, W](v, t) { // TODO - FIXME - v t are wrong here - should be from vp 
    val obj = None
    def adverbs = vp.adverbs
    def prepositions = vp.prepositions
  }

  sealed trait Tense

  case object Present extends Tense
  case object Past extends Tense
  case object PresentParticiple extends Tense
  case object PastParticiple extends Tense
  case object Imperative extends Tense
}
