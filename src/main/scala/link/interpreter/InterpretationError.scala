package link.interpreter

import link.LinkError

case class InterpretationError(msg: String) extends LinkError(msg)
