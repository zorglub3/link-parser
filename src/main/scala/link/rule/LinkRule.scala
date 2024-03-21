package link.rule

object LinkRule {
  case class NormalForm(disjunction: List[LinkList]) {
    def mergeWith(other: NormalForm) = NormalForm(disjunction ++ other.disjunction)
  }

  def debugPrintNormalForm(n: NormalForm) {
    for(ll <- n.disjunction) {
      debugPrintLinkList(ll)
    }
  }

  case class LinkList(l: List[LeftLink], r: List[RightLink]) {
    def ++(l2: LinkList): LinkList = LinkList(l ++ l2.l, r ++ l2.r)
    def leftLinks = l.reverse
    def rightLinks = r.reverse
  }
  
  def debugPrintLinkList(ll: LinkList) {
    println(s"${ll.leftLinks.map(_.linkTag.name).mkString(" & ")} <-> ${ll.rightLinks.map(_.linkTag.name).mkString(" & ")}")
  }

  abstract class Link(val linkTag: LinkTag, val isLeft: Boolean)

  case class LeftLink(lt: LinkTag) extends Link(lt, true)
  case class RightLink(lt: LinkTag) extends Link(lt, false)
}

object LinkRuleSyntax {
  import LinkRule._

  def l(linkTag: LinkTag): NormalForm = 
    NormalForm(List(LinkList(List(LeftLink(linkTag)), List.empty)))

  def r(linkTag: LinkTag): NormalForm =
    NormalForm(List(LinkList(List.empty, List(RightLink(linkTag)))))

  def empty: NormalForm =
    NormalForm(List(LinkList(List.empty, List.empty)))

  def opt(r: NormalForm): NormalForm =
    empty | r

  implicit class LRSyntax(l: NormalForm) {
    def |(r: NormalForm): NormalForm = l.mergeWith(r)
    def &(r: NormalForm): NormalForm = {
      NormalForm(
        for {
          c1 <- l.disjunction
          c2 <- r.disjunction
        } yield c1 ++ c2)
      }
  }
}

trait LinkRuleSet[W] {
  import LinkRule._
  import LinkRuleSyntax._

  val wordRules = collection.mutable.HashMap.empty[W, NormalForm]

  def addLinkRule(w: W, r: NormalForm) {
    val newRule = wordRules.get(w).map { _ | r } getOrElse(r)
    wordRules.update(w, newRule)
  }

  implicit class RuleSyntax(word: W) {
    def :-(r: NormalForm) {
      addLinkRule(word, r)
    }
  }
}

