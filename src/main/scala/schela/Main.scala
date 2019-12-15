package schela

import java.io.File
import scalaz._
import Scalaz._

import schela.Repl._
import schela.SchemeEval._

object Main {
  def main(args: Array[String]): Unit = {
    val loadResult = loadFile("stdlib.scm".toList, prims)
    println(extractValue(trapError(loadResult.map(_._1.shows))))

    loadResult
      .fold(
        { _ => runRepl(Nil) },
        { case (_, env) => runRepl(env)}
      )
  }
}
