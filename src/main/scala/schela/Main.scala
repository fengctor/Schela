package schela

import scala.io.Source.fromInputStream

import scalaz._
import Scalaz._

import Parsez._
import SchelaEval._
import SchelaParse._
import Types._

object Main extends Repl {
  def main(args: Array[String]): Unit = {
    println("Welcome to Schela: a Scheme-like language written in Scala by Gary F.")

    // will not fail (unless I messed up in which case I want the program to error out)
    loadStdlib(prims) match {
      case Right((_, env)) => runRepl(env)
    }
  }

  def loadStdlib(env: Env): ThrowsError[(SVal, Env)] = {
    val parser: Parsez[List[SVal]] = spaces *> endBy(parseExpr, spaces1)
    val input: List[Char] = fromInputStream(
      getClass.getResourceAsStream("/stdlib.scm")
    ).toList
    fileParseAttempt("stdlib", parser, input, env)
  }
}
