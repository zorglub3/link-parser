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

  def guardNone(v: => Option[_]): Option[Unit] = {
    v.fold(Option( () ))(_ => Option.empty[Unit])  
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
    tokenTags(w).collectFirst {
      case EnglishWordTags.Person(x) => x
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

  def prepositions(w: Int): List[Predicate.PositionPredicate[NounPhrase[String], String]] = {
    (for {
      idx <- graphEdgeFrom(EnglishLinkTags.P)(w)
      pp  <- preposition(idx)
      rest = prepositions(idx)
    } yield pp :: rest) .getOrElse(List.empty)  
  }

  def nounPredicates(w: Int): List[NounPredicate[NounPhrase[String], String]] = {
    nounAdjectives(w) ++ prepositions(w)
  }

  def linkPredicate(v: Int): Option[NounPredicate[NounPhrase[String], String]] = {
    def adjective(): Option[NounPredicate[NounPhrase[String], String]] = {
      for {
        adj <- graphEdgeFrom(EnglishLinkTags.B)(v)
        _ <- guard(tokenHasTag(adj, EnglishWordTags.Adjective))
        root <- getAdjectiveRoot(adj)
        superlative = isSuperlative(adj)
      } yield Predicate.SimplePredicate(root, superlative)
    }    

    adjective() orElse (graphEdgeFrom(EnglishLinkTags.P)(v).flatMap(preposition _)) 
  }

  def interpretNP(w: Int): Option[NounPhrase[String]] = {
    if(tokenHasTag(w, EnglishWordTags.Noun)) {
      for {
        root <- getNounRoot(w)
        det  <- graphEdgeFrom(EnglishLinkTags.D)(w)
        preds = nounPredicates(w)
      } yield NounPhrase.Thing(words(det), isPlural(w), root, preds)
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
  
  def adverbs(w: Int): List[Predicate.Adverbial[NounPhrase[String], String]] = {
    def getAdverb(p: Int): Option[Predicate.Adverbial[NounPhrase[String], String]] = {
      for {
        _ <- guard(tokenHasTag(p, EnglishWordTags.Adverb))
      } yield Predicate.Adverbial[NounPhrase[String], String](words(p))
    }
    val edges = allEdgesFrom(EnglishLinkTags.A)(w)
    edges.flatMap(getAdverb _)
  }

  def verbPredicates(w: Int): List[VerbPredicate[NounPhrase[String], String]] = {
    adverbs(w) ++ prepositions(w)    
  }

  def interpretBaseVP(w: Int): Option[VerbPhrase[NounPhrase[String], String] with BaseVerbPhrase[NounPhrase[String], String]] = {
    if(tokenHasTag(w, EnglishWordTags.Verb)) {
      if(tokenHasTag(w, EnglishWordTags.Intransitive)) {
        for {
          root <- getVerbRoot(w)
          preds = verbPredicates(w)
          tense = verbTense(w)
        } yield VerbPhrase.IntransitiveVerbPhrase(root, tense, preds)
      } else if(tokenHasTag(w, EnglishWordTags.Transitive)) {
        for {
          root <- getVerbRoot(w)
          tense = verbTense(w)
          preds = verbPredicates(w)
          np   <- graphEdgeFrom(EnglishLinkTags.O)(w)
          obj  <- interpretNP(np)
        } yield VerbPhrase.TransitiveVerbPhrase(root, tense, obj, preds)
      } else if(tokenHasTag(w, EnglishWordTags.LinkVerb)) {
        for {
          root <- getVerbRoot(w)
          tense = verbTense(w)
          preds = verbPredicates(w)
          nounPred <- linkPredicate(w)
        } yield VerbPhrase.LinkVerbPhrase(root, tense, nounPred, preds)
      } else {
        None
      }
    } else {
      None
    }
  }

  def interpretVP(n: Int, w: Int): Option[VerbPhrase[NounPhrase[String], String]] = {
    def withHelpVerb() = {
      for {
        h  <- graphEdgeFrom(EnglishLinkTags.H)(w)
        _ <- guard(h > n)
        root <- getVerbRoot(h)
        tense = verbTense(h)
        vp <- interpretBaseVP(w)
      } yield VerbPhrase.HelpVerbPhrase(root, tense, vp)
    }

    def withoutHelpVerb() = {
      for {
        _ <- guardNone(graphEdgeFrom(EnglishLinkTags.H)(w))
        vp <- interpretBaseVP(w)
      } yield vp
    }

    withHelpVerb() orElse withoutHelpVerb()   
  }

  def question(h: Int): QuestionMode = {
    (for {
      q <- graphEdgeFrom(EnglishLinkTags.Q)(h)
      m <- QuestionMode.fromString(words(q))
    } yield m) .getOrElse(QuestionMode.YesNo)
  }

  def interpretQuestion(): Option[SimpleSentence[NounPhrase[String], String]] = {
    for {
      (n, v) <- graphEdge(EnglishLinkTags.S)
      _ <- guard(isActiveVerb(v))
      np <- interpretNP(n)
      h <- graphEdgeFrom(EnglishLinkTags.H)(v)
      _ <- guard(h < n)
      root <- getVerbRoot(h)
      tense = verbTense(h)
      vp <- interpretBaseVP(v)
      q = question(h)
    } yield SimpleSentence.Question(q, np, vp) 
  }

  def interpretImperative(): Option[SimpleSentence[NounPhrase[String], String]] = {
    for {
      (w, v) <- graphEdge(EnglishLinkTags.W)
      _ <- guard(tokenHasTag(w, EnglishWordTags.Wall))
      _ <- guard(tokenHasTag(v, EnglishWordTags.Root))
      _ <- guard(tokenHasTag(v, EnglishWordTags.Verb))
      vp <- interpretVP(-1, v) 
    } yield SimpleSentence.Imperative(vp)
  }

  def interpretStatement(): Option[SimpleSentence[NounPhrase[String], String]] = {
    for {
      (n, v) <- graphEdge(EnglishLinkTags.S)
      _ <- guard(isActiveVerb(v))
      np <- interpretNP(n)
      vp <- interpretVP(n, v)
    } yield SimpleSentence.Statement(np, vp)
  }

  def interpretS(): Option[SimpleSentence[NounPhrase[String], String]] = {
    interpretImperative() orElse interpretStatement() orElse interpretQuestion()
  }
}

object EnglishInterpreter {
  def apply(r: ParseResult[String]): Option[SimpleSentence[NounPhrase[String], String]] = {
    val int = new EnglishInterpreter(r)
    int.interpretS()
  }
}
