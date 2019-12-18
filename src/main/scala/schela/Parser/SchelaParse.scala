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
    } yield SString(x.toSeq)
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
  lazy val parseList: Parsez[SVal] = sepBy(parseExpr, spaces1).map(SList)

  // Needs to be lazy to avoid recursive definition problems
  lazy val parseDottedList: Parsez[SVal] =
    for {
      vs <- endBy(parseExpr, spaces1)
      v <- char('.') *> spaces1 *> parseExpr
    } yield SDottedList(vs, v)

  val parseQuoted: Parsez[SVal] =
    for {
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

  /** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **
   Functions for preprocessing text before running it through the parser
   ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **/

  // TODO: is this what Lenses are for?
  private def toNextQuote(text: S): (S, S) = text match {
    case Nil => (Nil, Nil)
    case '\\' +: escaped +: cs =>
      val (acc, rest) = toNextQuote(cs)
      ('\\' +: escaped +: acc, rest)
    case '"' :: cs => (Nil, cs)
    case c :: cs =>
      val (acc, rest) = toNextQuote(cs)
      (c +: acc, rest)
  }

  def deleteComments(text: S): S = text match {
    case Nil => Nil
    case '"' +: cs =>
      val (fromQuote, afterQuote) = toNextQuote(cs)
      ('"' +: fromQuote) ++ ('"' +: deleteComments(afterQuote))
    case ';' +: cs => cs.dropWhile(!Set('\n', '\r').contains(_)) match {
      case Nil => Nil
      case rest => deleteComments(rest)
    }
    case c +: cs => c +: deleteComments(cs)
  }

  def parensMatch(text: S, stack: List[Char]): Boolean = (text, stack) match {
    case ('(' +: xs, _) => parensMatch(xs, '(' :: stack)
    case ('[' +: xs, _) => parensMatch(xs, '[' :: stack)
    case (')' +: xs, '(' :: ss) => parensMatch(xs, ss)
    case (')' +: xs, _) => false
    case (']' +: xs, '[' :: ss) => parensMatch(xs, ss)
    case (']' +: xs, _) => false
    case (_ +: xs, _) => parensMatch(xs, stack)
    case (Nil, Nil) => true
    case _ => false
  }

  def assimilateParens(text: S): S = text.map {
    case '[' => '('
    case ']' => ')'
    case c => c
  }
}
