package schela

import scalaz._
import Scalaz._

sealed trait SError

object SError {
  implicit val lispErrorShow: Show[SError] = new Show[SError] {
    override def shows(f: SError): String = f match {
      case NumArgs(expected, found) => s"Expected $expected args; found values ${found.map(_.shows).mkString(" ")}"
      case TypeMismatch(expected, found) => s"Invalid type: expected $expected, found $found"
      case Parser(err) => s"Parse error at $err"
      case BadSpecialForm(msg, v) => s"$msg: ${v.shows}"
      case NotAFunction(msg, fName) => s"$msg: $fName"
      case FileNotFound(fileName) => s"""File "$fileName" not found"""
      case UnboundVar(msg, vName) => s"$msg: $vName"
        // default??
    }
  }
}

final case class NumArgs(expected: Int, found: List[SVal]) extends SError
final case class TypeMismatch(expected: String, found: SVal) extends SError
final case class Parser(err: String) extends SError
final case class BadSpecialForm(msg: String, v: SVal) extends SError
final case class NotAFunction(msg: String, fName: String) extends SError
final case class UnboundVar(msg: String, vName: String) extends SError
final case class FileNotFound(fileName: String) extends SError
final case class Default(msg: String) extends SError

