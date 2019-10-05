package schela

import java.io.File

import schela.Repl._

object Main extends App {
  load("stdlib.scm".toList)
  runRepl
}
