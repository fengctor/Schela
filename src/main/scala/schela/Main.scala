package schela

import java.io.File
import scalaz._
import Scalaz._

import schela.Repl._
import schela.SchemeEval._

object Main {
  def main(args: Array[String]): Unit = {
    println(extractValue(trapError(loadFile("stdlib.scm".toList).map(_.shows))))
    runRepl()
  }
}
