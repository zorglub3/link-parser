package link.writer

sealed trait Punctuation

object Punctuation {
  case object Period extends Punctuation
  case object Comma extends Punctuation
  case object Exclamation extends Punctuation
  case object QuestionMark extends Punctuation
  case object Elipsis extends Punctuation

  def stringValue(p: Punctuation): String = {
    p match {
      case Period => "."
      case Comma => ","
      case Exclamation => "!"
      case QuestionMark => "?"
      case Elipsis => "..."
    }
  }
}
