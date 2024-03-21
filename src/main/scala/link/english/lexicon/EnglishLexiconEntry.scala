package link.english.lexicon

import link.rule.LinkRule

trait EnglishLexiconEntry {
  def linkRules: List[(String, LinkRule.NormalForm)]
  def words: List[String]
}
