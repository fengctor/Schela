package schela

import scala.io.Source._
import scala.collection._
import scalaz._
import Scalaz._

import schela.Parsez._
import schela.SchemeEval._
import schela.SchemeParse._
import schela.SchemePrimitives._
import schela.Types._

object Repl {

  val env: mutable.Map[String, LispVal] =
    mutable.Map().addAll(
      primitives.map { case (v, func) => (v, LPrimitiveFunc(func)) }
    )

  def getVar(name: String, closure: Option[mutable.Map[String, LispVal]] = None): ThrowsError[LispVal] =
    if (closure.exists(_.isDefinedAt(name)))
      closure.get(name).point[ThrowsError]
    else if (env.isDefinedAt(name))
      env(name).point[ThrowsError]
    else
      (UnboundVar("Getting an unbound variable", name): LispError)
        .raiseError[ThrowsError, LispVal]

  def setVar(name: String, value: LispVal, closure: Option[mutable.Map[String, LispVal]] = None): ThrowsError[LispVal] =
    if (closure.exists(_.isDefinedAt(name))) {
      closure.get(name) = value
      value.point[ThrowsError]
    } else if (env.isDefinedAt(name)) {
      env(name) = value
      value.point[ThrowsError]
    } else {
      (UnboundVar("Setting an unbound variable", name): LispError)
        .raiseError[ThrowsError, LispVal]
    }

  def defineVar(
    name: String,
    value: LispVal,
    closure: Option[mutable.Map[String, LispVal]] = None
  ): ThrowsError[LispVal] = {
    closure.getOrElse(env)(name) = value
    value.point[ThrowsError]
  }

  def bindVars(closure: mutable.Map[String, LispVal], vs: List[(String, LispVal)]): mutable.Map[String, LispVal] = {
    val newClosure = closure.clone()
    newClosure.addAll(vs)
    newClosure
  }

  def load(fileName: List[Char]): ThrowsError[LispVal] = {
    val parser: Parsez[List[LispVal]] = endBy(parseExpr, spaces)
    val src = scala.io.Source.fromFile(fileName.mkString)
    val input = src.toList
    src.close()
    val result = runParser(parser, input) match {
      case -\/(err) => (Parser(err): LispError).raiseError[ThrowsError, LispVal]
      case \/-(values) => values.map(eval(_)).sequence.map(LList)   // traverse messes up order of eval...
    }
    result
  }

  def runRepl(): Unit = {
    print("Schela>>> ")
    val in: List[Char] = scala.io.StdIn.readLine().toList
    if (in != "quit") {
      val evaled = (readExpr(in) >>= (expr => eval(expr))).map(_.shows)
      println(extractValue(trapError(evaled)))
      runRepl()
    }
  }
}
