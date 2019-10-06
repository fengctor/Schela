package schela

import scalaz._
import Scalaz._
import scala.Function.const
import Types._

sealed abstract class Parsez[+A] {
  def parse(s: S): List[(A, S)]

  // better looking syntax
  def <|>[B >: A](other: => Parsez[B]): Parsez[B] = Parsez.parserInstance.plus(this, other)
}

object Parsez {

  def apply[A](parseFn: S => List[(A, S)]): Parsez[A] = new Parsez[A] {
    def parse(s: S): List[(A, S)] = parseFn(s)
  }

  def runParser[A](parser: Parsez[A], s: List[Char]): String \/ A = {
    def parensMatch(text: List[Char], round: Int = 0, square: Int = 0): Boolean = text match {
      case '(' :: xs => parensMatch(xs, round + 1, square)
      case '[' :: xs => parensMatch(xs, round, square + 1)
      case ')' :: xs => if (round == 0) false else parensMatch(xs, round - 1, square)
      case ']' :: xs => if (square == 0) false else parensMatch(xs, round, square - 1)
      case _ :: xs => parensMatch(xs, round, square)
      case Nil => round == 0 && square == 0
    }

    def assimilateParens(text: List[Char]): List[Char] =
      text.map {
        case '[' => '('
        case ']' => ')'
        case c => c
      }

    if (parensMatch(s)) {
      parser.parse(assimilateParens(s)) match {
        case List((res, Nil)) => res.right
        case List((_, rs)) => s"stream left over: ${rs.mkString}".left
        case x => s"parser error: $x".left
      }
    } else {
      "mismatched parens".left
    }
  }

  val item: Parsez[Char] = Parsez {
    case Nil => Nil
    case c :: cs => List((c, cs))
  }

  implicit val parserInstance: Monad[Parsez] with Alternative[Parsez] =
    new Monad[Parsez] with Alternative[Parsez] {
      override def plus[A](a: Parsez[A], b: => Parsez[A]): Parsez[A] = Parsez { s =>
        a.parse(s) match {
          case Nil => b.parse(s)
          case x => x
        }
      }

      override def bind[A, B](fa: Parsez[A])(f: A => Parsez[B]): Parsez[B] = Parsez { s =>
        fa.parse(s) >>= { case (a, s2) => f(a).parse(s2) }
      }

      override def point[A](a: => A): Parsez[A] = Parsez(s => List((a, s)))

      override def empty[A]: Parsez[A] = Parsez(const(Nil))
    }

  def satisfy(p: Char => Boolean): Parsez[Char] = item >>= { c =>
    if (p(c)) c.point[Parsez] else Parsez(const(Nil))
  }

  def char(c: Char): Parsez[Char] = satisfy(_ == c)

  val letter: Parsez[Char] = satisfy(c => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
  val digit: Parsez[Char] = satisfy(c => '0' <= c && c <= '9')

  def oneOf(cs: List[Char]): Parsez[Char] = satisfy(cs.contains(_))

  def noneOf(cs: List[Char]): Parsez[Char] = satisfy(!cs.contains(_))

  def seq(s: List[Char]): Parsez[List[Char]] = s match {
    case Nil => List.empty.point[Parsez]
    case c :: cs => for {
      x <- char(c)
      xs <- seq(cs)
    } yield x :: xs
  }

  val symbol: Parsez[Char] = oneOf("!#$%&|*+-/:<=>?@^_~".toList)

  def many[A](p: Parsez[A]): Parsez[List[A]] = many1(p) <|> List.empty.point[Parsez]

  def many1[A](p: Parsez[A]): Parsez[List[A]] = for {
    c <- p
    cs <- many(p)
  } yield c :: cs

  def skipMany[A](p: Parsez[A]): Parsez[Unit] = many(p) *> ().point[Parsez]

  def skipMany1[A](p: Parsez[A]): Parsez[Unit] = p *> skipMany(p)

  def sepBy[A, S](p: Parsez[A], sep: Parsez[S]): Parsez[List[A]] =
    sepBy1(p, sep) <|> List.empty.point[Parsez]

  def sepBy1[A, S](p: Parsez[A], sep: Parsez[S]): Parsez[List[A]] = for {
    a <- p
    as <- many(sep *> p)
  } yield a :: as

  def endBy[A, S](p: Parsez[A], sep: Parsez[S]): Parsez[List[A]] = many(for {
    a <- p
    _ <- sep
  } yield a)

  def space: Parsez[Char] = oneOf(List(' ', '\t', '\n', '\r', '\f'))

  def spaces: Parsez[Unit] = skipMany1(space)

  val escape: Parsez[Char] = char('\\') >>= { _ =>
    Parsez {
      case '"' :: cs => List(('"', cs))
      case 't' :: cs => List(('\t', cs))
      case 'n' :: cs => List(('\n', cs))
      case 'r' :: cs => List(('\r', cs))
      case 'f' :: cs => List(('\f', cs))
      case '\\' :: cs => List(('\\', cs))
      case _ => Nil
    }
  }
}
