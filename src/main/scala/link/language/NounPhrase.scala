package link.language

abstract class NounPhrase[W]

object NounPhrase {
  case class Pronoun[W](person: Int, plural: Boolean, gender: Option[Gender]) extends NounPhrase[W]
  case class Thing[W](plural: Boolean, thing: W) extends NounPhrase[W] 
    
  trait Gender
  case object MaleGender extends Gender
  case object FemaleGender extends Gender
}
