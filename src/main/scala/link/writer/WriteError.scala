package link.writer

import link.LinkError

case class WriteError[W](word: W, context: String) extends LinkError(s"Missing word $word in context $context")
