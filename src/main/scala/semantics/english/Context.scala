package semantics.english

import link.LinkError
import amr.{AMR, Fix, Role}

trait Context[E] {
  import Context._
  
  type NL[X] = AMR.Node[String, Label, X]
  type NE[X] = Either[E, AMR.Node[String, Label, X]]

  // TODO - smells like cats!
  def traverseRoles[Err, A[_], B[_]](
    roles: Map[Role, Fix[A]],
    f: Fix[A] => Either[Err, Fix[B]],
  ): Either[Err, Map[Role, Fix[B]]] = {
    @scala.annotation.tailrec
    def t(v: List[(Role, Fix[A])], acc: List[(Role, Fix[B])]): Either[Err, List[(Role, Fix[B])]] = {
      v match {
        case Nil => Right(acc.reverse)
        case h :: tail => {
          f(h._2) match {
            case Left(err) => Left(err)
            case Right(v2) => t(tail, (h._1 -> v2) :: acc)
          }
        }
      }
    }

    t(roles.toList, List.empty).map(_.toMap)
  }

  def lookupEntity(e: E): Either[ContextError, Fix[NL]]
  def lookupPronoun(w: String, roles: Map[Role, Fix[NE]]): Either[ContextError, Fix[NE]]
  def lookupNoun(w: String, roles: Map[Role, Fix[NE]]): Either[ContextError, Fix[NE]]

  def toL(v: Fix[NE]): Either[ContextError, Fix[NL]] = {
    v.unfix match {
      case Left(entity) => lookupEntity(entity)
      case Right(AMR.Node(w, l, roles)) => {
        val ns = traverseRoles(roles, toL)
        ns.map { newRoles => Fix(AMR.Node(w, l, newRoles)) }
      }
    } 
  }

  def toE(v: Fix[NL]): Either[ContextError, Fix[NE]] = {
    v.unfix match {
      case AMR.Node(w, Some(Label.Pronoun), roles) => traverseRoles(roles, toE).flatMap(lookupPronoun(w, _))
      case AMR.Node(w, Some(Label.Noun), roles) => traverseRoles(roles, toE).flatMap(lookupNoun(w, _))
      case AMR.Node(w, l, roles) => {
        val ns = traverseRoles(roles, toE)
        ns.map { newRoles => { 
          val r: NE[Fix[NE]] = Right(AMR.Node(w, l, newRoles))
          Fix(r)
        } }
      }
    }
  }
}

object Context {
  case class ContextError(msg: String) extends LinkError(msg)
}
