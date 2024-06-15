package link.language

abstract class NounPhrase[W]

object NounPhrase {
  case class Pronoun[W](person: Int, plural: Boolean, gender: Option[Gender]) extends NounPhrase[W]
  case class Thing[W](determiner: W, plural: Boolean, thing: W, predicates: List[NounPredicate[NounPhrase[W], W]]) extends NounPhrase[W] 
    
  trait Gender
  case object MaleGender extends Gender
  case object FemaleGender extends Gender
}
