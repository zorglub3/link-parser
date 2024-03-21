package link.tokenizer

import collection.mutable.{HashSet, HashMap, MultiMap, Builder, Set => MutableSet}

trait TokenLexicon[W] {
  def lookup(str: String): Option[List[W]]
  def concat(tokens: List[String]): List[String]
  def leftWall: W
  def rightWall: W
}

class StringTokenLexiconBuilder {
  val tokens = HashSet[String]()

  val concatTokens =
    new HashMap[String, MutableSet[List[String]]] with MultiMap[String, List[String]]

  def addStringToken(t: String) {
    tokens.add(t)
  }

  def addConcatToken(parts: Seq[String]) {
    concatTokens.addBinding(parts.head, parts.tail.toList)
  }

  def addToken(t: String) {
    if(t.contains('_')) {
      val parts = t.split("_")
      addConcatToken(parts)
    } 

    addStringToken(t)
  }

  def result: TokenLexicon[String] = new TokenLexicon[String] {
    val tokenSet = tokens.clone()
    val concatTokensMap = concatTokens.clone()

    def lookup(t: String): Option[List[String]] =
      if(tokenSet.contains(t)) { Some(List(t)) } else { None }

    def concat(tokens: List[String]): List[String] = {
      def matchList(head: String, pattern: List[String], tail: List[String]): Option[(String, List[String])] = {
        if(tail.startsWith(pattern)) {
          Some((head +: pattern).mkString("_") -> tail.drop(pattern.length))
        } else {
          None
        }
      }

      @annotation.tailrec
      def findFirst(tails: List[List[String]], f: List[String] => Option[(String, List[String])]): Option[(String, List[String])] = {
        tails match {
          case Nil => None
          case h :: t => {
            f(h) match {
              case None => findFirst(t, f)
              case x: Some[(String, List[String])] => x
            }
          }
        }
      }

      @annotation.tailrec
      def iterate(acc: Builder[String, List[String]], rest: List[String]): List[String] = {
        rest match {
          case Nil => acc.result.reverse
          case h :: t => {
            val tails = concatTokensMap.getOrElse(h, Set.empty).toList.sortBy(_.length).reverse

            findFirst(tails, matchList(h, _, t)) match {
              case Some((token, rest2)) => iterate(acc += token, rest2)
              case None => iterate(acc += h, t)
            }
          }
        }
      }

      iterate(List.newBuilder[String], tokens).reverse
    }

    def leftWall: String = Tokenizer.LEFT_WALL
    def rightWall: String = Tokenizer.RIGHT_WALL
  }
}
