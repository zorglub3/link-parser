package link.language

import link.LinkError

trait ContextMapper[A, B] {
  def mapNP(np: A): Either[ContextMapper.UnmappedObject[A], (B, ContextMapper[A, B])]

  // def ++(b: ContextMapper[A, B]): ContextMapper[A, B]
}

object ContextMapper {
  final case class UnmappedObject[N](obj: N) extends LinkError("unmapped object")

  def fromFnOption[A, B](fn: A => Option[B]): ContextMapper[A, B] = {
    new ContextMapper[A, B] {
      def mapNP(np: A): Either[ContextMapper.UnmappedObject[A], (B, ContextMapper[A, B])] = 
        fn(np).map { x => Right((x, this))} .getOrElse(Left(UnmappedObject(np)))
    }
  }
}
