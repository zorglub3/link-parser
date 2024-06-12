package link.language

trait ContextMapper[A, B] {
  def mapNP(np: A): Either[ContextMapper.UnmappedObject[A], (B, ContextMapper[A, B])]

  def ++(b: ContextMapper[A, B]): ContextMapper[A, B]
}

object ContextMapper {
  final case class UnmappedObject[N](obj: N) 
}
