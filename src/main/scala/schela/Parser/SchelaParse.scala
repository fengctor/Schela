package schela

import scalaz.Scalaz.{char => _, _}
import Parsez._
import Types._

import scala.Function.const
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object SchelaParse {

  lazy val parseAtom: Parsez[SVal] = for {
    c  <- letter <|> symbol
    cs <- many(letter <|> digit <|> symbol)
  } yield c :: cs match {
    case List('#', 't') => SBool(true)
    case List('#', 'f') => SBool(false)
    case x => SAtom(x)
  }

  lazy val parseChar: Parsez[SVal] = {
    val specialChar: Parsez[SVal] = seq("space".toList).map(const(SChar(' '))) <|> seq("newline".toList).map(const(SChar('\n')))

    val nested = for {
      _ <- char('#')
      _ <- char('\\')
    } yield specialChar <|> item.map(SChar)

    nested.join
  }

  lazy val parseString: Parsez[SVal] = {
    for {
      _ <- char('"')
      x <- many(escape <|> satisfy(_ != '"'))
      _ <- char('"')
    } yield SString(x.toSeq)
  }

  lazy val parseNumber: Parsez[SVal] =
    for {
      neg <- optional('0', char('-'))
      digs <- many1(digit)
    } yield {
      val numValue = digs.foldLeft(0) ((a, c) => 10 * a + (c - '0'))
      SNumber(if (neg == '-') -1 * numValue else numValue)
    }

  lazy val parseList: Parsez[SVal] = sepBy(parseExpr, spaces1).map(SList)

  lazy val parseDottedList: Parsez[SVal] =
    for {
      vs <- endBy(parseExpr, spaces1)
      v <- char('.') *> spaces1 *> parseExpr
    } yield SDottedList(vs, v)

  lazy val parseQuoted: Parsez[SVal] =
    for {
      _ <- char('\'')
      x <- parseExpr
    } yield SList(List(SAtom("quote".toList), x))

  val parseExpr: Parsez[SVal] = {
    lazy val parseParenList: Parsez[SVal] = for {
      _ <- char('(')
      x <- parseList
      _ <- char(')')
    } yield x

    lazy val parseParenDottedList: Parsez[SVal] = for {
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

  // todo: queue and tailrec
  @tailrec
  def deleteComments(text: S, q: Queue[Char] = Queue(), quote: Boolean = false): S = text match {
    case Nil => q
    case '\\' +: escaped +: cs => deleteComments(cs, q :+ '\\' :+ escaped, quote)
    case '"' +: cs =>deleteComments(cs, q :+ '"', !quote)
    case ';' +: cs if !quote =>
      val commentDeleted = cs.dropWhile(!List('\n', '\r').contains(_))
      deleteComments(commentDeleted, q, quote)
    case c +: cs => deleteComments(cs, q :+ c, quote)
  }

  @tailrec
  def parensMatch(text: S, stack: List[Char] = Nil): Boolean = (text, stack) match {
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

  def roundifyParens(text: S): S = text.map {
    case '[' => '('
    case ']' => ')'
    case c => c
  }
}
