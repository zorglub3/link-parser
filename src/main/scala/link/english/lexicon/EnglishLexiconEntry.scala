package link.english.lexicon

import link.rule.{LinkRule, WordTag}

trait EnglishLexiconEntry {
  def linkRules: List[(String, LinkRule.NormalForm)]
  def words: List[String]
  def wordTags: List[(String, List[WordTag])]
}
