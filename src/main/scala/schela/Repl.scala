package schela

import scala.annotation.tailrec

import scalaz._
import Scalaz._

import Parsez.{ spaces, runParser }
import SchelaEval._
import SchelaParse._
import Types._


trait Repl {
  def readExpr(s: List[Char]): ThrowsError[SVal] = {
    val parser = for {
      _    <- spaces
      expr <- parseExpr
      _    <- spaces
    } yield expr

    val commentsRemoved = deleteComments(s)

    if (parensMatch(commentsRemoved)) {
      runParser(parser, roundifyParens(commentsRemoved)) match {
        case Left(err) => Parser(err).raiseError
        case Right(value) => value.point[ThrowsError]
      }
    } else {
      Parser("mismatched parens").raiseError
    }
  }

  @tailrec
  final def runRepl(env: Env): Unit = {
    print("Schela> ")
    val in: List[Char] = scala.io.StdIn.readLine().toList
    if (in != "quit".toList) {
      val evalResult = readExpr(in) >>= (expr => eval(expr, env))
      println(extractValue(trapError(evalResult.map(_._1.shows))))

      evalResult match {
        case Left(_) => runRepl(env)
        case Right((_, newEnv)) => runRepl(newEnv)
      }
    }
  }
}
