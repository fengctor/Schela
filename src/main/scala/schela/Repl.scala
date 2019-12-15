package schela

import scalaz._
import Scalaz._
import SchelaEval._
import SchelaParse._
import Types._

trait Repl {
  def runRepl(env: Env): Unit = {
    print("Schela>>> ")
    val in: List[Char] = scala.io.StdIn.readLine().toList
    if (in != "quit".toList) {
      val evalResult = readExpr(in) >>= (expr => eval(expr, env))
      println(extractValue(trapError(evalResult.map(_._1.shows))))

      evalResult
        .fold(
          { _ => runRepl(env) },
          { case (_, newEnv) => runRepl(newEnv)}
        )
    }
  }
}
