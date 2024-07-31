package semantics

import link.parser.ParseResult
import amr.{AMR, Fix}

trait InterpreterAMR[W, L] {
  type N[X] = AMR.Node[W, L, X]
  type T = AMR[N]
  type F = Fix[N]
  
  def interpret(parseResult: ParseResult[W]): Either[InterpretationError, T]
}


