package link.english.interpreter

import link.parser.ParseResult
import link.language._
import link.english.lexicon.{EnglishWordTags, EnglishLinkTags}

class EnglishInterpreter(result: ParseResult[String]) {
  import result._
  
  def guard(v: => Boolean): Option[Unit] = {
    if(v) {
      Some( () )
    } else {
      None
    }
  }

  def isActiveVerb(w: Int): Boolean = {
    val tags = tokenTags(w)

    tags.exists { 
      case EnglishWordTags.Present => true
      case EnglishWordTags.Past => true
      case _ => false
    }
  }

  def getVerbRoot(w: Int): Option[String] = {
    tokenTags(w).collectFirst { case EnglishWordTags.VerbRoot(root) => root }
  }

  def getNounRoot(w: Int): Option[String] = {
    tokenTags(w).collectFirst { case EnglishWordTags.NounRoot(root) => root }
  }

  def pronounPerson(w: Int): Option[Int] = {
    // TODO can be optimized - goes through all word tags three times
    if(tokenHasTag(w, EnglishWordTags.Person(1))) {
      Some(1)
    } else if(tokenHasTag(w, EnglishWordTags.Person(2))) {
      Some(2)
    } else if(tokenHasTag(w, EnglishWordTags.Person(3))) {
      Some(3)
    } else {
      None
    }
  }

  def isPlural(w: Int): Boolean =
    tokenHasTag(w, EnglishWordTags.Plural)
    
  def pronounGender(w: Int): Option[NounPhrase.Gender] = {
    if(tokenHasTag(w, EnglishWordTags.MaleGender)) {
      Some(NounPhrase.MaleGender)
    } else if(tokenHasTag(w, EnglishWordTags.FemaleGender)) {
      Some(NounPhrase.FemaleGender)
    } else {
      None
    }
  }
  
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

  def interpretImperative(): Option[SimpleSentence[NounPhrase[String], String]] = {
    for {
      (w, v) <- graphEdge(EnglishLinkTags.W)
      _ <- guard(tokenHasTag(w, EnglishWordTags.Wall))
      _ <- guard(tokenHasTag(v, EnglishWordTags.Root))
      _ <- guard(tokenHasTag(v, EnglishWordTags.Verb))
      vp <- interpretVP(v) 
    } yield SimpleSentence.Imperative(vp)
  }

  def interpretStatement(): Option[SimpleSentence[NounPhrase[String], String]] = {
    for {
      (n, v) <- graphEdge(EnglishLinkTags.S)
      _ <- guard(isActiveVerb(v))
      np <- interpretNP(n)
      vp <- interpretVP(v)
    } yield SimpleSentence.Statement(np, vp)
  }

  def interpretLinkStatement(): Option[SimpleSentence[NounPhrase[String], String]] = {
    for {
      (n, v) <- graphEdge(EnglishLinkTags.S)
      _ <- guard(isLinkVerb(v))
      np <- interpretNP(n)
      (v, p) <- interpretLV(v)
    } yield SimpleSentence.LinkStatement(np, v, p)
  }
  
  def interpretS(): Option[SimpleSentence[NounPhrase[String], String]] = {
    interpretImperative() orElse interpretStatement() orElse interpretLinkStatement()
  }
}
