package link.parser

import link.LinkError

case class ParseError[W](token: W, msg: String) extends LinkError(msg)
