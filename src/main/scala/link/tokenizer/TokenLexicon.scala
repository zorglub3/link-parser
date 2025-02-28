package link.tokenizer

import collection.mutable.{HashSet, Builder, MultiDict}
import link.rule.WordTag

// TODO simplify - with using the cats based tokenizer
trait TokenLexicon[W] {
  def lookup(str: String): Option[Vector[W]]
  def lookupOne(str: String): Option[W]
  def tags(token: W): Seq[WordTag]
  def concat(tokens: List[String]): List[String]
  def concatToken(token: String): List[List[String]]
  def leftWall: W
  def rightWall: W
}

// TODO make "ignoreCase" optional
class StringTokenLexiconBuilder {
  val ignoreCase = true
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
    val tt = if(ignoreCase) { t.toLowerCase() } else { t }

    if(tt.contains('_')) {
      val parts = tt.split("_")
      addConcatToken(parts.toList)
    } 

    addStringToken(tt)
  }

  def addTokenTag(t: String, tag: WordTag): Unit = {
    val tt = if(ignoreCase) { t.toLowerCase() } else { t }

    tokenTags.addOne(tt -> tag)
  }

  def result: TokenLexicon[String] = new TokenLexicon[String] {
    val ic = ignoreCase
    val tokenSet = tokens.clone()
    val concatTokensMap = concatTokens.collect { x => x }
    val tags = tokenTags.collect { x => x }

    def lookup(t: String): Option[Vector[String]] = {
      val tt = if(ic) { t.toLowerCase() } else { t }

      if(tokenSet.contains(tt)) { Some(Vector(t)) } else { None }
    }

    def lookupOne(t: String): Option[String] = {
      val tt = if(ic) { t.toLowerCase() } else { t }
      if(tokenSet.contains(tt)) { Some(t) } else { None }
    }

    def concatToken(t: String): List[List[String]] = {
      concatTokensMap.get(t).toList
    }

    def tags(token: String): List[WordTag] = { 
      val tt = if(ic) { token.toLowerCase() } else { token }

      tags.get(tt).toList
    }
    
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

      val tt = if(ic) { tokens.map(_.toLowerCase()) } else { tokens }
      iterate(List.newBuilder[String], tt).reverse
    }

    def leftWall: String = Tokenizer.LEFT_WALL
    def rightWall: String = Tokenizer.RIGHT_WALL
  }
}
