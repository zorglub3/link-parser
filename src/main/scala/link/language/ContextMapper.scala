package link.language

import link.LinkError

trait ContextMapper[A, B, +Repr] {
  def mapNP(np: A): Either[ContextMapper.UnmappedObject[A], (B, Repr)]
}

object ContextMapper {
  final case class UnmappedObject[N](obj: N) extends LinkError("unmapped object")

  case class FnOptionMapper[A, B](fn: A => Option[B]) extends ContextMapper[A, B, FnOptionMapper[A, B]] {
    def mapNP(np: A): Either[UnmappedObject[A], (B, FnOptionMapper[A, B])] = 
      fn(np).map { x => Right((x, this))} .getOrElse(Left(UnmappedObject(np)))
  }

  def fromFnOption[A, B](fn: A => Option[B]): FnOptionMapper[A, B] = {
    FnOptionMapper(fn)
  }
}
