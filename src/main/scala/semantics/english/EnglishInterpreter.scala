package semantics.english

import link.parser.ParseResult
import link.english.lexicon.{EnglishWordTags, EnglishLinkTags}
import amr.{AMR, Role}
import semantics._

class EnglishInterpreter extends InterpreterAMR[String, Label] {
  val nodeSyntax = new AMR.SimpleSyntax[String, Label]

  // guards for for-comprehensions
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

  // syntactic sugar for ParseResult
  implicit class ParseResultSyntax(pr: ParseResult[String]) {
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

  // the workhorse
  def interpretSimple(pr: ParseResult[String]): Option[F] = {
    import nodeSyntax._
    
    def prepositionRole(pp: String): Role = {
      pp match {
        case "to" => Role.Common.Destination
        case "in" => Role.Common.Location
        case "from" => Role.Common.Source
        case "through" => Role.Common.Path
        case "for" => Role.Common.Beneficiary
        case "about" => Role.Common.Topic
        case "with" => Role.Common.Instrument // TODO - beware "I ate with a friend" - the friend is not a tool!
        case _ => Role.Prep(pp)
      }
    }

    def interpretPrep(w: Int): Option[(Role, F)] = {
      for {
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Preposition)) 
        idx <- pr.graphEdgeFrom(EnglishLinkTags.R)(w)
        np <- interpretNP(idx)
        pp <- pr.getWord(w).map(_.toLowerCase())
        role = prepositionRole(pp)
      } yield role -> np
    }

    def interpretDirection(w: Int): Option[(Role, F)] = {
      for {
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Direction))
        word <- pr.getWord(w).map(_.toLowerCase())
      } yield (Role.Common.Direction -> word)
    }

    def interpretPronoun(w: Int): Option[F] = {
      for {
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Pronoun))
        word <- pr.getWord(w).map(_.toLowerCase())
        person <- pr.pronounPerson(w)
        plural = pr.isPlural(w)
        gender = pr.pronounGender(w)
      } yield (word / Label.Pronoun).leaf
    }

    def interpretProperNP(w: Int): Option[F] = 
      None // TODO stub

    def interpretDeterminer(w: Int): Option[List[(Role, F)]] = {
      def getNodes(word: String): List[(Role, F)] = {
        word match {
          case "a" => List(Role.Common.Definite -> "-")
          case "an" => List(Role.Common.Definite -> "-")
          case "the" => List(Role.Common.Definite -> "+")
          case "every" => List(Role.Common.Quant -> "every")
          case "some" => List(Role.Common.Quant -> "some")
          case "all" => List(Role.Common.Quant -> "all")
          case "no" => List(Role.Common.Quant -> "none")
          case "each" => List(Role.Common.Quant -> "each")
          case _ => List.empty
        }
      }

      for {
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Determiner))
        word <- pr.getWord(w).map(_.toLowerCase())
      } yield getNodes(word)    
    }

    def interpretStdNP(w: Int): Option[F] = {
      // TODO predicates
      for {
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Noun))
        word <- pr.getWord(w).map(_.toLowerCase())
        det <- pr.graphEdgeFrom(EnglishLinkTags.D)(w)
        nodes <- interpretDeterminer(det)
        plural = pr.isPlural(w)
      } yield {
        val pluralSign: F = if(plural) "+" else "-"
        (word / Label.Noun).withNodesList((Role.Common.Plural -> pluralSign) :: nodes)
      }   
    }

    def interpretNP(w: Int): Option[F] = {
      interpretPronoun(w) orElse interpretProperNP(w) orElse interpretStdNP(w)
    }

    def question(n: Int, h: Int): F => F = {
      (for {
        _ <- guard(h < n)
        q = pr.graphEdgeFrom(EnglishLinkTags.Q)(h).flatMap(pr.getWord(_).map(_.toLowerCase()))
      } yield { node: F =>
        val qnode = ("a" / Label.Unknown).leaf
        val qmode = (Role.Common.Mode -> "question")
        
        q match {
          case None => node.addRole(Role.Common.Polarity -> qnode).addRole(Role.Common.Mode -> "question")
          case Some("where") => node.addRole(Role.Common.Location -> qnode).addRole(Role.Common.Mode -> "question")
          case Some("why") => node.addRole(Role.Common.Purpose -> qnode).addRole(Role.Common.Mode -> "question")
          case Some("when") => node.addRole(Role.Common.Purpose -> qnode).addRole(Role.Common.Mode -> "question")
          case Some("how") => node.addRole(Role.Common.Manner -> qnode).addRole(Role.Common.Mode -> "question")
          case _ => node.addRole(Role.Common.Mode -> "question")
        }
      }) .getOrElse(identity)
    }

    def interpretHelpVerb(n: Int, w: Int, arg1: F): Option[F] = {
      for {
        h <- pr.graphEdgeFrom(EnglishLinkTags.H)(w)
        q = question(n, h)
        root <- pr.getVerbRoot(h)
        word <- pr.getWord(h).map(_.toLowerCase())
      } yield q((word / Label.VP(root)).withNodes(Role.Core.Arg1 -> arg1))
    }

    def interpretSimpleVerb(w: Int): Option[F] = {
      for {
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Transitive) || pr.tokenHasTag(w, EnglishWordTags.Intransitive))
        root <- pr.getVerbRoot(w)
        word <- pr.getWord(w).map(_.toLowerCase())
        np =  pr.graphEdgeFrom(EnglishLinkTags.O)(w)
        obj = np.flatMap(interpretNP(_)).map(Role.Core.Arg1 -> _).toList
      } yield (word / Label.VP(root)).withNodesList(obj)
    }

    def interpretVP(n: Int, w: Int): Option[F] = {
      interpretSimpleVerb(w)
        .map(mainVP => interpretHelpVerb(n, w, mainVP).getOrElse(mainVP))
    }

    def interpretStatement(): Option[F] = {
      for {
        (n, v) <- pr.graphEdge(EnglishLinkTags.S)
        np <- interpretNP(n)
        vp <- interpretVP(n, v)
      } yield {
        import nodeSyntax._
      
        vp.addRole(Role.Core.Arg0 -> np)
      }
    }

    def interpretImperative(): Option[F] = {
      for {
        (w, v) <- pr.graphEdge(EnglishLinkTags.W)
        _ <- guard(pr.tokenHasTag(w, EnglishWordTags.Wall))
        _ <- guard(pr.tokenHasTag(v, EnglishWordTags.Root))
        _ <- guard(pr.tokenHasTag(v, EnglishWordTags.Verb))
        vp <- interpretVP(-1, v)
      } yield vp.addRole(Role.Common.Mode -> "imperative")
    }

    interpretStatement() orElse interpretImperative()
  }

  def interpret(result: ParseResult[String]): Either[InterpretationError, T] = {
    interpretSimple(result)
      .toRight(InterpretationError("Could not make Abstract Meaning Representation for sentence"))
      .map(AMR.apply)
  }

  def interpretList(results: List[ParseResult[String]]): Either[InterpretationError, List[T]] = {
    results.flatMap(interpretSimple(_)) match {
      case Nil => Left(InterpretationError("Could not make AMR for sentence"))
      case h :: t => Right((h :: t).map(AMR.apply))
    }
  }

  def pp(amr: T): String = nodeSyntax.pp(amr)
}

