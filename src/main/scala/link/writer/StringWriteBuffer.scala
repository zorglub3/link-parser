package link.writer

case class StringWriteBuffer(
  makeSpace: Boolean, 
  capitalize: Boolean, 
  strings: List[String]
) extends WriteBuffer[String, StringWriteBuffer] {
  def addWord[Y >: String](w: Y): StringWriteBuffer = {
    w match {
      case s: String => {
        val ww = if(capitalize) { s.capitalize } else { s }
        val ll = if(makeSpace) { " " :: strings } else { strings }

        StringWriteBuffer(true, false, ww :: ll)
      }
      case _ => this
    }
  }

  def addPunctuation(p: Punctuation): StringWriteBuffer = {
    addWord(Punctuation.stringValue(p))
  }
}

object StringWriteBuffer {
  def empty = StringWriteBuffer(false, true, List.empty)
}
