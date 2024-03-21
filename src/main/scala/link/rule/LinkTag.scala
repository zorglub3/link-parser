package link.rule

class LinkTag(val name: String) {
  def merge(tag2: LinkTag): LinkTag = ???

  def matches(prefix: String): Boolean = name.startsWith(prefix)
  def matches(prefix: LinkTag): Boolean = name.startsWith(prefix.name) || prefix.name.startsWith(name)

  def simplify: LinkTag = new LinkTag(name.take(1))
}
