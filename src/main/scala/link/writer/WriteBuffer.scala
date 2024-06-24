package link.writer

trait WriteBuffer[+W, +Repr] {
  def addWord[Y >: W](w: Y): Repr
  def addPunctuation(p: Punctuation): Repr
}
