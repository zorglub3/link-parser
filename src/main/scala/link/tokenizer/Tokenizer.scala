package link.tokenizer

import java.util.StringTokenizer

class Tokenizer[W](lexicon: TokenLexicon[W], delimiters: String) {
  def apply(str: String): Either[UnrecognizedTokens, Vector[W]] = {
    val stringTokens = new StringTokenizer(str, delimiters)
    val builder = List.newBuilder[String]
    val missing = List.newBuilder[String]

    while(stringTokens.hasMoreTokens) {
      builder += stringTokens.nextToken
    }

    val tokens = lexicon.concat(builder.result())

    for(t <- tokens) {
      if(lexicon.lookup(t).isEmpty) {
        missing += t
      }
    }
    
    val unrecognized = missing.result()

    if(unrecognized.isEmpty) {
      Right(lexicon.leftWall +: tokens.toVector.flatMap(lexicon.lookup(_).getOrElse(Vector.empty)))
    } else {
      Left(UnrecognizedTokens(unrecognized))
    }
  }

  import cats.data.StateT
  import cats.data.IndexedStateT
  import cats.Eval

  type Tokenize[A] = StateT[Eval, TokenizerState, A]
  
  case class TokenizerState(
    tokens: List[W],
    unrecognized: List[String]
  ) {
    def addToken(t: W): TokenizerState = copy(tokens = t :: tokens)
    def addUnrecognized(t: String): TokenizerState = copy(unrecognized = t :: unrecognized)
  }

  def init(): TokenizerState = TokenizerState(List.empty, List.empty)

  def scanTokens(tokens: List[String]): Tokenize[Unit] = {
    tokens match {
      case Nil => StateT.pure(())
      case h::t => {
        val concatTokens = lexicon.concatToken(h)
        concatTokens.find(t.startsWith(_)) match {
          case Some(tail) => continueScan((h :: tail).mkString("_"), t.drop(tail.length))
          case None => continueScan(h, t)
        }
      }
    }
  }

  def unrecognize(token: String): Tokenize[Unit] = StateT.modify(_.addUnrecognized(token))
  def token(token: W): Tokenize[Unit] = StateT.modify(_.addToken(token))

  def continueScan(t: String, rest: List[String]): Tokenize[Unit] = {
    for {
      _ <- lexicon.lookupOne(t).fold(unrecognize(t))(token(_))
      _ <- scanTokens(rest)
    } yield ()
  }

  type EitherError[A] = Either[List[String], A]
  def toEither[A](f: Tokenize[A]): IndexedStateT[EitherError, TokenizerState, List[W], Unit] = 
    f.transformF { evalState =>
      evalState.value match {
        case (TokenizerState(_, h::t), _) => Left(h :: t)
        case (TokenizerState(l, Nil), _) => Right((l.reverse, ()))
      }
    }
}

object Tokenizer {
  val LEFT_WALL: String = "////"
  val RIGHT_WALL: String = "\\\\\\\\"
}
