package link.english.interpreter

import link.parser.ParseResult
import link.language._
import link.english.lexicon.{EnglishWordTags, EnglishLinkTags}
import link.interpreter.InterpretationError

class EnglishInterpreter {
  def guard(v: => Boolean): Option[Unit] = {
    if(v) {
      Some( () )
    } else {
      None
    }
  }

  def guardNone(v: => Option[_]): Option[Unit] = {
    v.fold(Option( () ))(_ => Option.empty[Unit])  
  }

  def isActiveVerb(result: ParseResult[String], w: Int): Boolean = {
    import result._
    
    val tags = tokenTags(w)

    tags.exists { 
      case EnglishWordTags.Present => true
      case EnglishWordTags.Past => true
      case _ => false
    }
  }

  def getVerbRoot(result: ParseResult[String], w: Int): Option[String] = {
    import result._
    
    tokenTags(w).collectFirst { case EnglishWordTags.VerbRoot(root) => root }
  }

  def getNounRoot(result: ParseResult[String], w: Int): Option[String] = {
    import result._
    
    tokenTags(w).collectFirst { case EnglishWordTags.NounRoot(root) => root }
  }

  def getAdjectiveRoot(result: ParseResult[String], w: Int): Option[String] = {
    import result._
    
    tokenTags(w).collectFirst { case EnglishWordTags.AdjectiveRoot(root) => root }
  }

  def isSuperlative(result: ParseResult[String], w: Int): Boolean = {
    import result._
    
    tokenHasTag(w, EnglishWordTags.Superlative)
  }

  def pronounPerson(result: ParseResult[String], w: Int): Option[Int] = {
    import result._
    
    tokenTags(w).collectFirst {
      case EnglishWordTags.Person(x) => x
    }
  }

  def isPlural(result: ParseResult[String], w: Int): Boolean = {
    import result._
    tokenHasTag(w, EnglishWordTags.Plural)
  }
    
  def isLinkVerb(result: ParseResult[String], w: Int): Boolean = ???
  
  def pronounGender(result: ParseResult[String], w: Int): Option[NounPhrase.Gender] = {
    import result._
    
    if(tokenHasTag(w, EnglishWordTags.MaleGender)) {
      Some(NounPhrase.MaleGender)
    } else if(tokenHasTag(w, EnglishWordTags.FemaleGender)) {
      Some(NounPhrase.FemaleGender)
    } else {
      None
    }
  }
  
  def preposition(result: ParseResult[String], w: Int): Option[Predicate.PositionPredicate[NounPhrase[String], String]] = {
    import result._
    
    for {
      _ <- guard(tokenHasTag(w, EnglishWordTags.Preposition))
      idx <- graphEdgeFrom(EnglishLinkTags.R)(w)
      np <- interpretNP(result, idx)
    } yield Predicate.PositionPredicate(words(w), np)
  }

  def nounAdjectives(result: ParseResult[String], w: Int): List[Predicate[NounPhrase[String], String]] = {
    import result._
    
    (for {
      adj  <- graphEdgeLeft(EnglishLinkTags.J)(w)
      _    <- guard(tokenHasTag(adj, EnglishWordTags.Adjective))
      root <- getAdjectiveRoot(result, adj)
      superlative = isSuperlative(result, adj)
      rest =  nounAdjectives(result, adj)
    } yield Predicate.SimplePredicate[NounPhrase[String], String](root, superlative) :: rest) .getOrElse(List.empty)
  }

  def prepositions(result: ParseResult[String], w: Int): List[Predicate.PositionPredicate[NounPhrase[String], String]] = {
    import result._
    
    (for {
      idx <- graphEdgeFrom(EnglishLinkTags.P)(w)
      pp  <- preposition(result, idx)
      rest = prepositions(result, idx)
    } yield pp :: rest) .getOrElse(List.empty)  
  }

  def nounPredicates(result: ParseResult[String], w: Int): List[Predicate[NounPhrase[String], String]] = {
    nounAdjectives(result, w) ++ prepositions(result, w)
  }

  def linkPredicate(result: ParseResult[String], v: Int): Option[Predicate[NounPhrase[String], String]] = {
    import result._ 

    def adjective(): Option[Predicate[NounPhrase[String], String]] = {
      for {
        adj <- graphEdgeFrom(EnglishLinkTags.B)(v)
        _ <- guard(tokenHasTag(adj, EnglishWordTags.Adjective))
        root <- getAdjectiveRoot(result, adj)
        superlative = isSuperlative(result, adj)
      } yield Predicate.SimplePredicate(root, superlative)
    }    

    adjective() orElse (graphEdgeFrom(EnglishLinkTags.P)(v).flatMap(preposition(result, _))) 
  }

  def interpretNP(result: ParseResult[String], w: Int): Option[NounPhrase[String]] = {
    import result._
    
    if(tokenHasTag(w, EnglishWordTags.Noun)) {
      for {
        root <- getNounRoot(result, w)
        det  <- graphEdgeFrom(EnglishLinkTags.D)(w)
        preds = nounPredicates(result, w)
      } yield NounPhrase.Thing(words(det), isPlural(result, w), root, preds)
    } else if(tokenHasTag(w, EnglishWordTags.Pronoun)) {
      for {
        person <- pronounPerson(result, w)
        plural =  isPlural(result, w)
        gender =  pronounGender(result, w)
      } yield NounPhrase.Pronoun(person, plural, gender)
    } else if(tokenHasTag(w, EnglishWordTags.ProperNoun)) {
      ???
    } else {
      None
    }
  }

  def verbTense(result: ParseResult[String], w: Int): VerbPhrase.Tense = {
    import result._
    
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
  
  def adverbs(result: ParseResult[String], w: Int): List[Predicate.Adverbial[NounPhrase[String], String]] = {
    import result._
    
    def getAdverb(p: Int): Option[Predicate.Adverbial[NounPhrase[String], String]] = {
      for {
        _ <- guard(tokenHasTag(p, EnglishWordTags.Adverb))
      } yield Predicate.Adverbial[NounPhrase[String], String](words(p))
    }
    val edges = allEdgesFrom(EnglishLinkTags.A)(w)
    edges.flatMap(getAdverb _)
  }

  def verbPredicates(result: ParseResult[String], w: Int): List[Predicate[NounPhrase[String], String]] = {
    adverbs(result, w) ++ prepositions(result, w)    
  }

  def interpretBaseVP(result: ParseResult[String], w: Int): Option[VerbPhrase[NounPhrase[String], String] with BaseVerbPhrase[NounPhrase[String], String]] = {
    import result._
    
    if(tokenHasTag(w, EnglishWordTags.Verb)) {
      if(tokenHasTag(w, EnglishWordTags.Intransitive)) {
        for {
          root <- getVerbRoot(result, w)
          preds = verbPredicates(result, w)
          tense = verbTense(result, w)
        } yield VerbPhrase.IntransitiveVerbPhrase(root, tense, preds)
      } else if(tokenHasTag(w, EnglishWordTags.Transitive)) {
        for {
          root <- getVerbRoot(result, w)
          tense = verbTense(result, w)
          preds = verbPredicates(result, w)
          np   <- graphEdgeFrom(EnglishLinkTags.O)(w)
          obj  <- interpretNP(result, np)
        } yield VerbPhrase.TransitiveVerbPhrase(root, tense, obj, preds)
      } else if(tokenHasTag(w, EnglishWordTags.LinkVerb)) {
        for {
          root <- getVerbRoot(result, w)
          tense = verbTense(result, w)
          preds = verbPredicates(result, w)
          nounPred <- linkPredicate(result, w)
        } yield VerbPhrase.LinkVerbPhrase(root, tense, nounPred, preds)
      } else {
        None
      }
    } else {
      None
    }
  }

  def interpretVP(result: ParseResult[String], n: Int, w: Int): Option[VerbPhrase[NounPhrase[String], String]] = {
    import result._
    
    def withHelpVerb() = {
      for {
        h  <- graphEdgeFrom(EnglishLinkTags.H)(w)
        _ <- guard(h > n)
        root <- getVerbRoot(result, h)
        tense = verbTense(result, h)
        vp <- interpretBaseVP(result, w)
      } yield VerbPhrase.HelpVerbPhrase(root, tense, vp)
    }

    def withoutHelpVerb() = {
      for {
        _ <- guardNone(graphEdgeFrom(EnglishLinkTags.H)(w))
        vp <- interpretBaseVP(result, w)
      } yield vp
    }

    withHelpVerb() orElse withoutHelpVerb()   
  }

  def question(result: ParseResult[String], h: Int): QuestionMode = {
    import result._
    
    (for {
      q <- graphEdgeFrom(EnglishLinkTags.Q)(h)
      m <- QuestionMode.fromString(words(q))
    } yield m) .getOrElse(QuestionMode.YesNo)
  }

  def interpretQuestion(result: ParseResult[String]): Option[SimpleSentence[NounPhrase[String], String]] = {
    import result._
    
    for {
      (n, v) <- graphEdge(EnglishLinkTags.S)
      _ <- guard(isActiveVerb(result, v))
      np <- interpretNP(result, n)
      h <- graphEdgeFrom(EnglishLinkTags.H)(v)
      _ <- guard(h < n)
      root <- getVerbRoot(result, h)
      tense = verbTense(result, h)
      vp <- interpretBaseVP(result, v)
      q = question(result, h)
    } yield SimpleSentence.Question(q, np, vp) 
  }

  def interpretImperative(result: ParseResult[String]): Option[SimpleSentence[NounPhrase[String], String]] = {
    import result._
    
    for {
      (w, v) <- graphEdge(EnglishLinkTags.W)
      _ <- guard(tokenHasTag(w, EnglishWordTags.Wall))
      _ <- guard(tokenHasTag(v, EnglishWordTags.Root))
      _ <- guard(tokenHasTag(v, EnglishWordTags.Verb))
      vp <- interpretVP(result, -1, v) 
    } yield SimpleSentence.Imperative(vp)
  }

  def interpretStatement(result: ParseResult[String]): Option[SimpleSentence[NounPhrase[String], String]] = {
    import result._
    
    for {
      (n, v) <- graphEdge(EnglishLinkTags.S)
      _ <- guard(isActiveVerb(result, v))
      np <- interpretNP(result, n)
      vp <- interpretVP(result, n, v)
    } yield SimpleSentence.Statement(np, vp)
  }

  def interpretS(result: ParseResult[String]): Option[SimpleSentence[NounPhrase[String], String]] = {
    interpretImperative(result) orElse interpretStatement(result) orElse interpretQuestion(result)
  }

  def interpret(results: List[ParseResult[String]]): Either[InterpretationError, List[SimpleSentence[NounPhrase[String], String]]] = {
    results.flatMap(interpretS _) match {
      case Nil => Left(InterpretationError("No valid interpretations"))
      case h :: t => Right(h :: t)
    }
  }
}
