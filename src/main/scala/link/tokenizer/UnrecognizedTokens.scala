package link.tokenizer

import link.LinkError

case class UnrecognizedTokens(tokens: List[String]) extends LinkError(s"Unrecognized tokens: $tokens")
