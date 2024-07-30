package semantics

import link.parser.ParseResult
import amr.AMR

trait InterpreterAMR[W, L] {
  def interpret(parseResult: ParseResult[W]): Either[InterpretationError, AMR[W, L, Nothing]]
}


