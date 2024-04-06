package link.rule

abstract class WordTag(val name: String) {
  override def equals(other: Any): Boolean = other match {
    case wt: WordTag => wt.name == name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode()
}
