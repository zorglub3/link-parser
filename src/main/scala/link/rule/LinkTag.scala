package link.rule

class LinkTag(val name: String) {
  def merge(tag2: LinkTag): LinkTag = ???

  def matches(prefix: String): Boolean = name.startsWith(prefix)
  def matches(prefix: LinkTag): Boolean = name.startsWith(prefix.name) || prefix.name.startsWith(name)

  def simplify: LinkTag = new LinkTag(name.take(1))

  override def toString(): String = s"LinkTag($name)"
  override def equals(other: Any): Boolean = other match {
    case l: LinkTag if l.name == name => true
    case _ => false
  }
  override def hashCode(): Int = name.hashCode()
}
