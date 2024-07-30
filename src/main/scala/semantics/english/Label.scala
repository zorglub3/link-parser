package semantics.english

sealed trait Label

object Label {
  final case class VP(root: String) extends Label

  case object Pronoun extends Label
  case object Noun extends Label
  case object Unknown extends Label // for 'amr-unknown', ie, questions
}
