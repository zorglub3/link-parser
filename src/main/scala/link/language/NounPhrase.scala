package link.language

abstract class NounPhrase[W]

object NounPhrase {
  case class Pronoun[W](pronounCase: PronounCase, person: Int, plural: Boolean, gender: Option[Gender]) extends NounPhrase[W]
  case class Thing[W](plural: Boolean, thing: W) extends NounPhrase[W] 
    
  trait PronounCase
  case object Nominative extends PronounCase
  case object Accusative extends PronounCase

  trait Gender
  case object MaleGender extends Gender
  case object FemaleGender extends Gender
}
