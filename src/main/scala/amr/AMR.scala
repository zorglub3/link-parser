package amr

final case class Fix[F[_]](unfix: F[Fix[F]])

case class AMR[R[_]](root: Fix[R]) {
  def mapNodes[A[_]](f: Fix[R] => Fix[A]): AMR[A] = AMR(f(root))  
  def mapNodesE[E, A[_]](f: Fix[R] => Either[E, Fix[A]]): Either[E, AMR[A]] = f(root).map(AMR.apply)
  def mapNodesOpt[E, A[_]](f: Fix[R] => Option[Fix[A]]): Option[AMR[A]] = f(root).map(AMR.apply)
}

object AMR {
  case class Node[W, L, N](
    word: W, 
    label: Option[L],
    roles: Map[Role, N]
  )

  class SimpleSyntax[W, L] {
    import scala.language.implicitConversions

    type N[X] = Node[W, L, X]
    type T = AMR[N]

    implicit class WordSyntax(w: W) {
      def /(l: L) = LabelSyntax(w, Some(l))
    }

    case class LabelSyntax(w: W, l: Option[L]) {
      def withNodes(roles: (Role, Fix[N])*): Fix[N] = 
        Fix(Node(w, l, roles.toMap))

      def withNodesList(roles: List[(Role, Fix[N])]): Fix[N] =
        Fix(Node(w, l, roles.toMap))

      def leaf: Fix[N] =
        Fix(Node(w, l, Map.empty))
    }

    implicit def wordToNodeWOLabel(w: W): Fix[N] =
      Fix(Node(w, None, Map.empty))

    implicit class NodeSyntax(n: Fix[N]) {
      def addRole(role: (Role, Fix[N])): Fix[N] = 
        n.unfix match { case Node(w, l, roles) => 
          Fix(Node(w, l, roles + role))
        }
    }

    def pp(amr: AMR[N]): String = {
      def ppNodes(indent: Int, roles: List[(Role, Fix[N])]): List[String] = {
        val sortedRoles = roles.sortBy(_._1.pp)
        sortedRoles.map { case (r, n) => ("  " * indent) ++ r.pp ++ " " ++ ppNode(indent, n) }
      }

      def ppNode(indent: Int, node: Fix[N]): String = {
        node.unfix match { 
          case Node(w, None, roles) => (s"($w)" :: ppNodes(indent + 1, roles.toList)) .mkString("\n")
          case Node(w, Some(l), roles) => (s"($w / $l)" :: ppNodes(indent + 1, roles.toList)) .mkString("\n")
        }
      }

      ppNode(0, amr.root)
    }
  }
}

object Demo {
  val syntax = new AMR.SimpleSyntax[String, String]
  import syntax._
  
  import Role.Core._
  
  val s1 = AMR(
    ("wants" / "want-01").withNodes(
      Arg0 -> ("boy" / "boy").withNodes(), 
      Arg1 -> ("believe" / "believe-01").withNodes(Arg0 -> "boy")
    )
  )
}
