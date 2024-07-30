package amr

case class AMR[Word, Label, Entity](
  root: AMR.Node[Word, Label, Entity],
) {
  def pp: String = {
    def ppNodes(indent: Int, roles: List[(Role, AMR.Node[Word, Label, Entity])]): List[String] = {
      val sortedRoles = roles.sortBy(_._1.pp)
      sortedRoles.map { case (r, n) => ("  " * indent) ++ r.pp ++ " " ++ ppNode(indent, n) }
    }

    def ppNode(indent: Int, node: AMR.Node[Word, Label, Entity]): String = {
      node match {
        case Left(e) => "<entity>"
        case Right(AMR.GraphNode(w, None, roles)) => { 
          (s"($w)" :: ppNodes(indent + 1, roles.toList)) .mkString("\n")
        }
        case Right(AMR.GraphNode(w, Some(l), roles)) => { 
          (s"($w / $l)" :: ppNodes(indent + 1, roles.toList)) .mkString("\n")
        }
      }
    }

    ppNode(0, root)
  }

  // TODO collect and collectFirst, iterators and other patterns
}

object AMR {
  type Node[W, L, E] = Either[E, GraphNode[W, L, E]]
  
  case class GraphNode[W, L, E](word: W, label: Option[L], roles: Map[Role, Node[W, L, E]])

  class Syntax[W, L, E] {
    import scala.language.implicitConversions
    
    implicit class WordSyntax(w: W) {
      def /(l: L) = LabelSyntax(w, Some(l))
    }

    case class LabelSyntax(word: W, label: Option[L]) {
      def nodes(roles: (Role, Node[W, L, E])*): Node[W, L, E] = 
        Right(GraphNode(word, label, roles.toMap))

      def withNodes(roles: List[(Role, Node[W, L, E])]): Node[W, L, E] =
        Right(GraphNode(word, label, roles.toMap))
    }

    implicit def wordToNodeWOLabel(w: W): Node[W, L, E] =
      Right(GraphNode(w, None, Map.empty))

    def amr(root: Node[W, L, E]) = AMR[W, L, E](root)

    implicit class NodeSyntax(node: Node[W, L, E]) {
      def addRole(role: (Role, Node[W, L, E])): Node[W, L, E] = {
        node.map { n => GraphNode(n.word, n.label, n.roles + role) }
      }
    }
  }
}

object Demo {
  val syntax = new AMR.Syntax[String, String, Nothing]
  import syntax._
  
  import Role.Core._
  
  val s1 = amr(
    ("wants" / "want-01").nodes(
      Arg0 -> ("boy" / "boy").nodes(), 
      Arg1 -> ("believe" / "believe-01").nodes(Arg0 -> "boy")
    )
  )
}
