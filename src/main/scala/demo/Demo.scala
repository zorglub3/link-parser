package demo

import link.tokenizer.Tokenizer
import link.parser.LinkParser
import link.english._
import link.graph.GraphInterpreter

class Demo {
  val builder = 
    new EnglishLexiconBuilder with StandardVerbs with StandardWords with StandardNouns
  val tokenLexicon = builder.tokenLexicon
  val tokenizer = new Tokenizer[String](tokenLexicon, " ")
  val parser = new LinkParser[String](builder.linkRules)

  def parseLinks(str: String) = {
    tokenizer(str.toLowerCase).map { tokens =>
      (new GraphInterpreter(tokens.toVector, tokenLexicon), parser.links(tokens))
    }
  }
}
