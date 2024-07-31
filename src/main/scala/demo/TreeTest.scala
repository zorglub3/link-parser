package demo

final case class Fix[F[_]](unfix: F[Fix[F]])

case class Tree[R[_]](root: Fix[R]) {
  def mapNodes[A[_]](f: Fix[R] => Fix[A]): Tree[A] = Tree(f(root))
}

object Tree {
  case class Node[L, N](label: L, children: List[N])

  type NS[X] = Node[String, X]

  type T1 = Tree[NS]

  type NE[X] = Either[Int, Node[String, X]]
  
  type T2 = Tree[NE]

  def gg(n: Fix[NS]): Fix[NS] = {
    n.unfix match { case Node(label, children) =>
      Fix(Node(label ++ label, children.map(gg)))
    }
  }

  def ff(n: Fix[NS]): Fix[NE] = {
    n.unfix match { case Node(label, children) =>
      val r: NE[Fix[NE]] = Right(Node(label, children.map(ff)))
      Fix(r)
      // Fix(Left(34))
      // Fix(Right(Node(label, children.map(ff))))
    }
  }

  // case class Node[+F[_[_], _], A](label: A, children: List[F[A]])

  // type N1[F[_]] = Node[F, String]
  // type T1 = Tree[N1, String]
} 
