package link.rule

import scala.collection.mutable.MultiDict

class RuleMap[W] {
  val entries: MultiDict[W, RuleMap.Entry] = MultiDict.empty

  def addEntry(word: W, wordTags: Seq[WordTag], linkRules: Seq[LinkRule.NormalForm]): Unit = {
    for {
      normalForm <- linkRules.toList
      linkList <- normalForm.disjunction
    } entries.addOne(word -> RuleMap.Entry(wordTags.toList, linkList))
  }

  def lookup(word: W): List[RuleMap.Entry] =
    entries.get(word).toList

  def lookupLinkRules(word: W): Option[LinkRule.NormalForm] = {
    val conjunction = entries.get(word).map(_.linkRule).toList

    if(conjunction.isEmpty) {
      None
    } else {
      Some(LinkRule.NormalForm(conjunction))
    }
  }
}

object RuleMap {
  case class Entry(
    wordTags: List[WordTag],
    linkRule: LinkRule.LinkList,
  )
}
