package link.writer

sealed trait Casus

object Casus {
  case object Nominative extends Casus
  case object Accusative extends Casus
  case object Possessive extends Casus
}
