package schela

import scala.collection.mutable
import scalaz._
import Scalaz._

import Types._

sealed trait LispVal {
}

object LispVal {
  implicit val lispValShow: Show[LispVal] = new Show[LispVal] {
    override def shows(f: LispVal): String = f match {
      case LAtom(name) => name.mkString

      case LList(vs) => s"(${vs.map(shows).mkString(" ")})"

      case LDottedList(vs, v) => s"(${vs.map(shows).mkString(" ")} . ${shows(v)})"

      case LNumber(n) => n.toString

      case LChar(' ') => "#\\space"
      case LChar('\n') => "#\\newline"
      case LChar(c) => s"#\\$c"

      case LString(s) => s.mkString("\"", "", "\"")

      case LBool(true) => "#t"
      case LBool(false) => "#f"

      case LPrimitiveFunc(_) => "<primitive>"

      case LFunc(args, varargs, body, _) =>
        s"(lambda (${args.mkString(" ")}${varargs.map(" . " + _).getOrElse("")}) ...)"
    }
  }
}

final case class LAtom(name: S) extends LispVal {
  override def toString: String = s"LAtom(${name.mkString})"
}
final case class LNumber(n: Int) extends LispVal
final case class LChar(c: Char) extends LispVal
final case class LString(s: S) extends LispVal {
  override def toString: String = s"""LString("${s.mkString}")"""
}
final case class LBool(b: Boolean) extends LispVal
final case class LList(vs: List[LispVal]) extends LispVal
final case class LDottedList(vs: List[LispVal], v: LispVal) extends LispVal
final case class LPrimitiveFunc(f: List[LispVal] => ThrowsError[LispVal]) extends LispVal
final case class LFunc(
  params: List[String],
  vararg: Option[String],
  body: List[LispVal],
  closure: mutable.Map[String, LispVal]
) extends LispVal