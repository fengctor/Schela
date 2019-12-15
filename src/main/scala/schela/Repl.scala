package schela

import java.io.FileNotFoundException

import scala.io.BufferedSource

import scalaz._
import Scalaz._

import schela.Parsez._
import schela.SchemeEval._
import schela.SchemeParse._
import schela.SchemePrimitives._
import schela.Types._

object Repl {

  val prims: List[(String, LispVal)] =
    primitives.map { case (v, func) => (v, LPrimitiveFunc(func)) }

  def getVar(name: String, env: List[(String, LispVal)]): ThrowsError[LispVal] =
    env.find(_._1 === name) match {
      case Some((_, value)) => value.point[ThrowsError]
      case _ => UnboundVar("Getting an unbound variable", name).raiseError
    }

  /* byebye impure
  def setVar(name: String, value: LispVal, closure: Option[mutable.Map[String, LispVal]] = None): ThrowsError[LispVal] =
    if (closure.exists(_.isDefinedAt(name))) {
      closure.get(name) = value
      value.point[ThrowsError]
    } else if (env.isDefinedAt(name)) {
      env(name) = value
      value.point[ThrowsError]
    } else {
      UnboundVar("Setting an unbound variable", name).raiseError
    }*/

  def defineVar(name: String, value: LispVal, env: List[(String, LispVal)]): List[(String, LispVal)] = {
    (name -> value) :: env
  }

  def bindVars(env: List[(String, LispVal)], vs: List[(String, LispVal)]): List[(String, LispVal)] = vs ++ env

  def loadFile(fileName: List[Char], env: List[(String, LispVal)]): ThrowsError[(LispVal, List[(String, LispVal)])] = {
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
          case Right(Nil) =>
            (LAtom(s"$fileStr loaded, but nothing happened".toList), env).point[ThrowsError]
          case Right(v :: vs) =>
            vs.foldLeft(eval(v, env)) { (acc, cur) =>
              acc >>= {
                case (_, resEnv) => eval(cur, resEnv)
              }
            } match {
              case Left(err) =>
                err.raiseError
              case Right((_, resEnv)) =>
                (LAtom(s"$fileStr loaded".toList), resEnv).point[ThrowsError]
            }
        }
    }
  }

  def runRepl(env: List[(String, LispVal)]): Unit = {
    print("Schela>>> ")
    val in: List[Char] = scala.io.StdIn.readLine().toList
    if (in != "quit".toList) {
      val evalResult = readExpr(in) >>= (expr => eval(expr, env))
      val evalShow = evalResult.map(_._1.shows)
      println(extractValue(trapError(evalShow)))

      evalResult
        .fold(
          { _ => runRepl(env) },
          { case (_, newEnv) => runRepl(newEnv)}
        )
    }
  }
}
