package schela

import scalaz._

import Types._

sealed trait SVal

object SVal {
  implicit val SValShow: Show[SVal] = new Show[SVal] {
    override def shows(f: SVal): String = f match {
      case SAtom(name) => name.mkString

      case SNumber(n) => n.toString

      case SChar(' ') => "#\\space"
      case SChar('\n') => "#\\newline"
      case SChar(c) => s"#\\$c"

      case SString(s) => s.mkString("\"", "", "\"")

      case SBool(true) => "#t"
      case SBool(false) => "#f"

      case SList(vs) => s"(${vs.map(shows).mkString(" ")})"

      case SDottedList(vs, v) => s"(${vs.map(shows).mkString(" ")} . ${shows(v)})"

      case SStruct(name, Nil) => name
      case SStruct(name, args) => s"($name ${args.map(shows).mkString(" ")})"

      case SPrimitiveFunc(_) => "<primitive>"

      case SFunc(nameOpt, args, varargs, _, _) =>
        val fnShow = s"(lambda (${args.mkString(" ")}${varargs.map(" . " + _).getOrElse("")}) ...)"
        nameOpt.fold(fnShow)(name => s"$name := $fnShow")

      case SUnit() => ""
    }
  }

  implicit val SValEqual: Equal[SVal] = Equal.equalA[SVal]
}

final case class SAtom(name: S) extends SVal
final case class SNumber(n: Int) extends SVal
final case class SChar(c: Char) extends SVal
final case class SString(s: S) extends SVal
final case class SBool(b: Boolean) extends SVal
final case class SList(vs: List[SVal]) extends SVal
final case class SDottedList(vs: List[SVal], v: SVal) extends SVal
final case class SStruct(name: String, args: List[SVal]) extends SVal
final case class SPrimitiveFunc(f: List[SVal] => ThrowsError[SVal]) extends SVal
final case class SFunc(
  name: Option[String],
  params: List[String],
  vararg: Option[String],
  body: List[SVal],
  env: Env
) extends SVal
final case class SUnit() extends SVal
