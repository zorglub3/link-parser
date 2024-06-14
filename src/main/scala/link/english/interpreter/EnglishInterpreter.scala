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

  def getAdjectiveRoot(w: Int): Option[String] = {
    tokenTags(w).collectFirst { case EnglishWordTags.AdjectiveRoot(root) => root }
  }

  def isSuperlative(w: Int): Boolean = {
    tokenHasTag(w, EnglishWordTags.Superlative)
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
    
  def isLinkVerb(w: Int): Boolean = ???
  
  def pronounGender(w: Int): Option[NounPhrase.Gender] = {
    if(tokenHasTag(w, EnglishWordTags.MaleGender)) {
      Some(NounPhrase.MaleGender)
    } else if(tokenHasTag(w, EnglishWordTags.FemaleGender)) {
      Some(NounPhrase.FemaleGender)
    } else {
      None
    }
  }
  
  def preposition(w: Int): Option[Predicate.PositionPredicate[NounPhrase[String], String]] = {
    for {
      _ <- guard(tokenHasTag(w, EnglishWordTags.Preposition))
      idx <- graphEdgeFrom(EnglishLinkTags.R)(w)
      np <- interpretNP(idx)
    } yield Predicate.PositionPredicate(words(w), np)
  }

  def nounAdjectives(w: Int): List[NounPredicate[NounPhrase[String], String]] = {
    (for {
      adj  <- graphEdgeLeft(EnglishLinkTags.J)(w)
      _    <- guard(tokenHasTag(adj, EnglishWordTags.Adjective))
      root <- getAdjectiveRoot(adj)
      superlative = isSuperlative(adj)
      rest =  nounAdjectives(adj)
    } yield Predicate.SimplePredicate[NounPhrase[String], String](root, superlative) :: rest) .getOrElse(List.empty)
  }

  def nounPrepositions(w: Int): List[NounPredicate[NounPhrase[String], String]] = {
    (for {
      idx <- graphEdgeFrom(EnglishLinkTags.P)(w)
      pp  <- preposition(idx)
      rest = nounPrepositions(idx)
    } yield pp :: rest) .getOrElse(List.empty)  
  }

  def nounPredicates(w: Int): List[NounPredicate[NounPhrase[String], String]] = {
    nounAdjectives(w) ++ nounPrepositions(w)
  }

  def interpretNP(w: Int): Option[NounPhrase[String]] = {
    if(tokenHasTag(w, EnglishWordTags.Noun)) {
      for {
        root <- getNounRoot(w)
        preds = nounPredicates(w)
        // TODO article
      } yield NounPhrase.Thing(isPlural(w), root, preds)
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

  def verbTense(w: Int): VerbPhrase.Tense = {
    if(tokenHasTag(w, EnglishWordTags.Present)) {
      VerbPhrase.Present
    } else if(tokenHasTag(w, EnglishWordTags.Past)) {
      VerbPhrase.Past
    } else if(tokenHasTag(w, EnglishWordTags.PresentParticiple)) {
      VerbPhrase.PresentParticiple
    } else if(tokenHasTag(w, EnglishWordTags.PastParticiple)) {
      VerbPhrase.PastParticiple
    } else {
      VerbPhrase.Imperative
    }
  }
  
  // TODO adverbials and prepositions
  def interpretVP(w: Int): Option[VerbPhrase[NounPhrase[String], String]] = {
    if(tokenHasTag(w, EnglishWordTags.Verb)) {
      if(tokenHasTag(w, EnglishWordTags.Intransitive)) {
        for {
          root <- getVerbRoot(w)
          tense = verbTense(w)
        } yield VerbPhrase.IntransitiveVerbPhrase(root, tense, List.empty)
      } else if(tokenHasTag(w, EnglishWordTags.Transitive)) {
        for {
          root <- getVerbRoot(w)
          tense = verbTense(w)
          np   <- graphEdgeFrom(EnglishLinkTags.O)(w)
          obj  <- interpretNP(np)
        } yield VerbPhrase.TransitiveVerbPhrase(root, tense, obj, List.empty)
      } else {
        None
      }
    } else {
      None
    }
  }

  def interpretLV(w: Int): Option[VerbPhrase.LinkVerbPhrase[NounPhrase[String], String]] = ???
  
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
      vp <- interpretLV(v)
    } yield SimpleSentence.Statement(np, vp)
  }
  
  def interpretS(): Option[SimpleSentence[NounPhrase[String], String]] = {
    interpretImperative() orElse interpretStatement() orElse interpretLinkStatement()
  }
}
