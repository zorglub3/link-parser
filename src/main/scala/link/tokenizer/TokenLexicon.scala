package link.tokenizer

import collection.mutable.{HashSet, Builder, MultiDict}
import link.rule.WordTag

trait TokenLexicon[W] {
  def lookup(str: String): Option[Vector[W]]
  def tags(token: W): Seq[WordTag]
  def concat(tokens: List[String]): List[String]
  def leftWall: W
  def rightWall: W
}

class StringTokenLexiconBuilder {
  val tokens = HashSet[String]()
  val tokenTags = MultiDict.empty[String, WordTag]

  val concatTokens =
    MultiDict.empty[String, List[String]]

  def addStringToken(t: String): Unit = {
    tokens.add(t)
  }

  def addConcatToken(parts: List[String]): Unit = {
    concatTokens.addOne(parts.head -> parts.tail.toList)
  }

  def addToken(t: String): Unit = {
    if(t.contains('_')) {
      val parts = t.split("_")
      addConcatToken(parts.toList)
    } 

    addStringToken(t)
  }

  def addTokenTag(t: String, tag: WordTag): Unit = {
    tokenTags.addOne(t -> tag)
  }

  def result: TokenLexicon[String] = new TokenLexicon[String] {
    val tokenSet = tokens.clone()
    val concatTokensMap = concatTokens.collect { x => x }
    val tags = tokenTags.collect { x => x }

    def lookup(t: String): Option[Vector[String]] =
      if(tokenSet.contains(t)) { Some(Vector(t)) } else { None }

    def tags(token: String): List[WordTag] = 
      tags.get(token).toList
    
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
          case Nil => acc.result().reverse
          case h :: t => {
            val tails = concatTokensMap.get(h).toList.sortBy(_.length)

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
