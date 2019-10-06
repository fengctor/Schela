package schela

import scalaz._
import Scalaz._

sealed trait LispError

object LispError {
  implicit val lispErrorShow: Show[LispError] = new Show[LispError] {
    override def shows(f: LispError): String = f match {
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

final case class NumArgs(expected: Int, found: List[LispVal]) extends LispError
final case class TypeMismatch(expected: String, found: LispVal) extends LispError
final case class Parser(err: String) extends LispError
final case class BadSpecialForm(msg: String, v: LispVal) extends LispError
final case class NotAFunction(msg: String, fName: String) extends LispError
final case class UnboundVar(msg: String, vName: String) extends LispError
final case class FileNotFound(fileName: String) extends LispError
final case class Default(msg: String) extends LispError

