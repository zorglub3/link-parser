package link.language.interpreter

import link.parser.ParseResult
import link.language._
import link.english.lexicon.{EnglishWordTags, EnglishLinkTags}

class EnglishInterpreter(result: ParseResult[String]) {
  import result._
  
  def getVerbRoot(w: Int): Option[String] = {
    tokenTags(w).collectFirst { case EnglishWordTags.VerbRoot(root) => root }
  }

  def getNounRoot(w: Int): Option[String] = {
    tokenTags(w).collectFirst { case EnglishWordTags.NounRoot(root) => root }
  }

  def pronounPerson(w: Int): Option[Int] = ???

  def isPlural(w: Int): Boolean =
    tokenHasTag(w, EnglishWordTags.Plural)
    
  def pronounGender(w: Int): Option[NounPhrase.Gender] = ???
  
  def interpretNP(w: Int): Option[NounPhrase[String]] = {
    if(tokenHasTag(w, EnglishWordTags.Noun)) {
      for(root <- getNounRoot(w)) yield NounPhrase.Thing(isPlural(w), root)
    } else if(tokenHasTag(w, EnglishWordTags.Pronoun)) {
      for {
        person <- pronounPerson(w)
        plural =  isPlural(w)
        gender =  pronounGender(w)
      } yield NounPhrase.Pronoun(person, plural, gender)
    } else if(tokenHasTag(w, EnglishWordTags.ProperNoun)) {
      ???
    } else {
      None
    }
  }

  def interpretVP(w: Int): Option[VerbPhrase[NounPhrase[String], String]] = {
    if(tokenHasTag(w, EnglishWordTags.Verb)) {
      if(tokenHasTag(w, EnglishWordTags.Intransitive)) {
        for(root <- getVerbRoot(w)) yield VerbPhrase.IntransitiveVerbPhrase(root)
      } else if(tokenHasTag(w, EnglishWordTags.Transitive)) {
        for {
          root <- getVerbRoot(w)
          np   <- graphEdgeFrom(EnglishLinkTags.O)(w)
          obj  <- interpretNP(np)
        } yield VerbPhrase.TransitiveVerbPhrase(root, obj)
      } else {
        None
      }
    } else {
      None
    }
  }
}
