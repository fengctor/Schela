package schela

import java.io.FileNotFoundException

import scala.io.BufferedSource
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
    mutable.Map() ++= primitives.map { case (v, func) => (v, LPrimitiveFunc(func)) }

  def getVar(name: String, closure: Option[mutable.Map[String, LispVal]] = None): ThrowsError[LispVal] =
    if (closure.exists(_.isDefinedAt(name)))
      closure.get(name).point[ThrowsError]
    else if (env.isDefinedAt(name))
      env(name).point[ThrowsError]
    else
      UnboundVar("Getting an unbound variable", name).raiseError

  def setVar(name: String, value: LispVal, closure: Option[mutable.Map[String, LispVal]] = None): ThrowsError[LispVal] =
    if (closure.exists(_.isDefinedAt(name))) {
      closure.get(name) = value
      value.point[ThrowsError]
    } else if (env.isDefinedAt(name)) {
      env(name) = value
      value.point[ThrowsError]
    } else {
      UnboundVar("Setting an unbound variable", name).raiseError
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
    newClosure ++= vs
  }

  def loadFile(fileName: List[Char]): ThrowsError[LispVal] = {
    def fromFileOpt(file: String): Option[BufferedSource] = {
      try {
        val src = scala.io.Source.fromFile(file)
        src.some
      } catch {
        case _: FileNotFoundException => none
      }
    }

    val fileStr = fileName.mkString
    val parser: Parsez[List[LispVal]] = endBy(parseExpr, spaces)
    val srcOpt = fromFileOpt(fileStr)
    val inputOpt = srcOpt.map(_.toList)

    srcOpt.foreach(_.close())

    inputOpt match {
      case None => FileNotFound(fileStr).raiseError
      case Some(input) =>
        runParser(parser, input) match {
          case Left(err) =>
            Parser(err).raiseError
          case Right(values) =>
            values.map(eval(_)).sequence match {  // traverse messes up order of eval...
              case Left(err) =>
                err.raiseError
              case Right(_) =>
                LAtom(s"$fileStr loaded".toList).point[ThrowsError]
            }
        }
    }
  }

  def runRepl(): Unit = {
    print("Schela>>> ")
    val in: List[Char] = scala.io.StdIn.readLine().toList
    if (in != "quit".toList) {
      val evaled = (readExpr(in) >>= (expr => eval(expr))).map(_.shows)
      println(extractValue(trapError(evaled)))
      runRepl()
    }
  }
}
