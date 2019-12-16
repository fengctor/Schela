package schela

import scalaz.Scalaz.{char => _, _}

import Parsez._
import Types._

import scala.Function.const

object SchelaParse {

  val parseAtom: Parsez[SVal] = for {
    c  <- letter <|> symbol
    cs <- many(letter <|> digit <|> symbol)
  } yield c :: cs match {
    case List('#', 't') => SBool(true)
    case List('#', 'f') => SBool(false)
    case x => SAtom(x)
  }

  val parseChar: Parsez[SVal] = {
    val specialChar: Parsez[SVal] = seq("space".toList).map(const(SChar(' '))) <|> seq("newline".toList).map(const(SChar('\n')))

    val nested = for {
      _ <- char('#')
      _ <- char('\\')
    } yield specialChar <|> item.map(SChar)

    nested.join
  }

  val parseString: Parsez[SVal] = {
    for {
      _ <- char('"')
      x <- many(escape <|> satisfy(_ != '"'))
      _ <- char('"')
    } yield SString(x)
  }

  val parseNumber: Parsez[SVal] =
    for {
      neg <- optional('0', char('-'))
      digs <- many1(digit)
    } yield {
      val numValue = digs.foldLeft(0) ((a, c) => 10 * a + (c - '0'))
      SNumber(if (neg == '-') -1 * numValue else numValue)
    }

  // Needs to be lazy to avoid recursive definition problems
  lazy val parseList: Parsez[SVal] = sepBy(parseExpr, spaces).map(SList)

  // Needs to be lazy to avoid recursive definition problems
  lazy val parseDottedList: Parsez[SVal] = for {
    vs <- endBy(parseExpr, spaces)
    v <- char('.') *> spaces *> parseExpr
  } yield SDottedList(vs, v)

  val parseQuoted: Parsez[SVal] = for {
    _ <- char('\'')
    x <- parseExpr
  } yield SList(List(SAtom("quote".toList), x))

  val parseExpr: Parsez[SVal] = {
    val parseParenList: Parsez[SVal] = for {
      _ <- char('(')
      x <- parseList
      _ <- char(')')
    } yield x

    val parseParenDottedList: Parsez[SVal] = for {
      _ <- char('(')
      x <- parseDottedList
      _ <- char(')')
    } yield x

    parseNumber <|>
      parseChar <|>
      parseAtom <|>
      parseString <|>
      parseQuoted <|>
      (parseParenList <|> parseParenDottedList)
  }

  def readExpr(s: List[Char]): ThrowsError[SVal] = {
    runParser(parseExpr, s) match {
      case Left(err) => Parser(err).raiseError
      case Right(value) => value.point[ThrowsError]
    }
  }
}
