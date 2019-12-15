package schela

import java.io.File
import scalaz._
import Scalaz._

import SchelaEval._

object Main extends Repl {
  def main(args: Array[String]): Unit = {
    val loadResult = loadFile("stdlib.scm".toList, prims)
    println(extractValue(trapError(loadResult.map(_._1.shows))))

    loadResult
      .fold(
        { _ => runRepl(prims) },
        { case (_, env) => runRepl(env)}
      )
  }
}
