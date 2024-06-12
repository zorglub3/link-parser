package link.english.lexicon

import link.rule.{LinkRule, WordTag}

trait EnglishLexiconEntry {
  def linkRules: List[(String, LinkRule.NormalForm)] = 
    wordEntries.map { entry => entry.word -> entry.linkRule }

  def words: List[String] = 
    wordEntries.map { _.word }

  def wordTags: List[(String, List[WordTag])] = 
    wordEntries.map { entry => entry.word -> entry.tags }

  val wordEntries: List[EnglishLexiconEntry.WordEntry]
}

object EnglishLexiconEntry {
  case class WordEntry(
    word: String,
    tags: List[WordTag],
    linkRule: LinkRule.NormalForm,
  )
}
