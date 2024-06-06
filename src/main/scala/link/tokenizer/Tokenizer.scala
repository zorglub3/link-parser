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
}

object Tokenizer {
  val LEFT_WALL: String = "////"
  val RIGHT_WALL: String = "\\\\\\\\"
}
