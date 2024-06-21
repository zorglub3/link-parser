package link.language

sealed abstract class NounPhrase[W]

object NounPhrase {
  final case class Pronoun[W](person: Int, plural: Boolean, gender: Option[Gender]) extends NounPhrase[W]
  final case class Thing[W](determiner: W, plural: Boolean, thing: W, predicates: List[NounPredicate[NounPhrase[W], W]]) extends NounPhrase[W] 
    
  trait Gender
  case object MaleGender extends Gender
  case object FemaleGender extends Gender
}
