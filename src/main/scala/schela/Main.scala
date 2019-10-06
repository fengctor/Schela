package schela

import java.io.File

import schela.Repl._

object Main extends App {
  loadFile("stdlib.scm".toList)
  runRepl
}
