package schela

import scala.Function.const
import scalaz._
import Scalaz.{char => _, _}

import schela.Parsez._
import schela.Types._

object SchemeParse {
// Lisp stuff

  val parseAtom: Parsez[LispVal] = for {
    c  <- letter <|> symbol
    cs <- many(letter <|> digit <|> symbol)
  } yield c :: cs match {
    case List('#', 't') => LBool(true)
    case List('#', 'f') => LBool(false)
    case x => LAtom(x)
  }

  val parseChar: Parsez[LispVal] = {
    val specialChar: Parsez[LispVal] = seq("space".toList).map(const(LChar(' '))) <|> seq("newline".toList).map(const(LChar('\n')))

    val nested = for {
      _ <- char('#')
      _ <- char('\\')
    } yield specialChar <|> item.map(LChar)

    nested.join
  }

  val parseString: Parsez[LispVal] = {
    for {
      _ <- char('"')
      x <- many(escape <|> satisfy(_ != '"'))
      _ <- char('"')
    } yield LString(x)
  }

  val parseNumber: Parsez[LispVal] = many1(digit).map { ns =>
    LNumber(ns.foldLeft(0) ((a, c) => 10 * a + (c - '0')))
  }

  // Needs to be lazy to avoid recursive definition problems
  lazy val parseList: Parsez[LispVal] = sepBy(parseExpr, spaces).map(LList)

  // Needs to be lazy to avoid recursive definition problems
  lazy val parseDottedList: Parsez[LispVal] = for {
    vs <- endBy(parseExpr, spaces)
    v <- char('.') *> spaces *> parseExpr
  } yield LDottedList(vs, v)

  val parseQuoted: Parsez[LispVal] = for {
    _ <- char('\'')
    x <- parseExpr
  } yield LList(List(LAtom("quote".toList), x))

  val parseExpr: Parsez[LispVal] = {
    val parseParenList: Parsez[LispVal] = for {
      _ <- char('(')
      x <- parseList
      _ <- char(')')
    } yield x

    val parseParenDottedList: Parsez[LispVal] = for {
      _ <- char('(')
      x <- parseDottedList
      _ <- char(')')
    } yield x

    parseChar <|>
      parseAtom <|>
      parseString <|>
      parseNumber <|>
      parseQuoted <|>
      (parseParenList <|> parseParenDottedList)
  }

  def readExpr(s: List[Char]): ThrowsError[LispVal] = runParser(parseExpr, s) match {
    case -\/(err) => (Parser(err): LispError).raiseError[ThrowsError, LispVal]
    case \/-(value) => value.point[ThrowsError]
  }
}