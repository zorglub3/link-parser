package amr.frame

import link.english.lexicon.EnglishWordTags
import link.parser.ParseResult

object ParseResultSyntax {
  implicit class Syntax(pr: ParseResult[String]) {
    def getVerbRoot(w: Int): Option[String] = 
      pr.collectTag(w) { case EnglishWordTags.VerbRoot(root) => root }

    def getNounRoot(w: Int): Option[String] =
      pr.collectTag(w) { case EnglishWordTags.NounRoot(root) => root }

    def getAdjectiveRoot(w: Int): Option[String] =
      pr.collectTag(w) { case EnglishWordTags.AdjectiveRoot(root) => root }

    def isSuperlative(w: Int): Boolean =
      pr.tokenHasTag(w, EnglishWordTags.Superlative)

    def pronounPerson(w: Int): Option[Int] =
      pr.collectTag(w) { case EnglishWordTags.Person(x) => x }

    def isPlural(w: Int): Boolean =
      pr.tokenHasTag(w, EnglishWordTags.Plural)

    def isLinkVerb(w: Int): Boolean = 
      pr.tokenHasTag(w, EnglishWordTags.LinkVerb)

    def pronounGender(w: Int): Option[String] = {
      pr.collectTag(w) {
        case EnglishWordTags.MaleGender => "male"
        case EnglishWordTags.FemaleGender => "female"
      }
    }
    
  }
}
