package link.language

sealed trait Tense

object Tense {
  case object Present extends Tense
  case object Past extends Tense
  case object PresentParticiple extends Tense
  case object PastParticiple extends Tense
  case object Imperative extends Tense
}
