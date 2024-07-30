package amr

sealed abstract class Role {
  def pp: String
}

case class InverseRole(role: RoleBase) extends Role {
  def pp = role.pp ++ "-of"
}

sealed abstract class RoleBase extends Role

object Role {
  sealed abstract class Core(name: String) extends RoleBase {
    def pp = ":" ++ name
  }

  object Core {
    case object Arg0 extends Core("ARG0") 
    case object Arg1 extends Core("ARG1")
    case object Arg2 extends Core("ARG2")
    case object Arg3 extends Core("ARG3")
    case object Arg4 extends Core("ARG4")
    case object Arg5 extends Core("ARG5")

    val Agent = Arg0
  }

  sealed abstract class Common(name: String) extends RoleBase {
    def pp = ":" ++ name
  }

  object Common {
    case object Accompanier extends Common("accompanier")
    case object Age extends Common("age")
    case object Beneficiary extends Common("beneficiary")
    case object Concession extends Common("concession")
    case object Condition extends Common("condition")
    case object ConsistsOf extends Common("consistsof")
    case object Definite extends Common("definite")
    case object Degree extends Common("degree")
    case object Destination extends Common("destination")
    case object Direction extends Common("direction")
    case object Domain extends Common("domain")
    case object Duration extends Common("duration")
    case object Example extends Common("example")
    case object Extent extends Common("extent")
    case object Frequency extends Common("frequency")
    case object Instrument extends Common("instrument")
    case object Li extends Common("li")
    case object Location extends Common("location")
    case object Manner extends Common("manner")
    case object Medium extends Common("medium")
    case object Mod extends Common("mod")
    case object Mode extends Common("mode")
    case object Name extends Common("name")
    case object Ord extends Common("ord")
    case object Part extends Common("part")
    case object Path extends Common("path")
    case object Plural extends Common("plural")
    case object Polarity extends Common("polarity")
    case object Polite extends Common("polite")
    case object Poss extends Common("pass")
    case object Purpose extends Common("purpose")
    case object Quant extends Common("quant")
    case object Range extends Common("range")
    case object Scale extends Common("scale")
    case object Source extends Common("source")
    case object Subevent extends Common("subevent")
    case object Time extends Common("time")
    case object Topic extends Common("topic")
    case object Unit extends Common("unit")
    case object Value extends Common("value")
    case object Wiki extends Common("wiki")
  }

  // TODO :opx roles for conjunctions

  case class Prep(preposition: String) extends RoleBase {
    def pp = "prep-" ++ preposition
  }
}
