package link.language

sealed trait Gender

object Gender {
  case object Male extends Gender
  case object Female extends Gender
}
