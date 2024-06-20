package link.language

sealed trait QuestionMode

object QuestionMode {
  case object YesNo extends QuestionMode
  case object When extends QuestionMode
  case object Where extends QuestionMode
  case object How extends QuestionMode
  case object Why extends QuestionMode

  def fromString(s: String): Option[QuestionMode] = {
    s match {
      case "when" => Some(When)
      case "where" => Some(Where)
      case "how" => Some(How)
      case "why" => Some(Why)
      case _ => None
    }
  }
}
