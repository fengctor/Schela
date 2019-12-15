package schela

import scalaz.Scalaz._
import scalaz._
import schela.Types.S

import scala.Function.const

sealed abstract class Parsez[+A] {
  def parse(s: S): List[(A, S)]

  // because I can't find a good typeclass like Haskell's Alternative in Scalaz
  def <|>[B >: A](other: => Parsez[B]): Parsez[B] = Parsez { s =>
    parse(s) match {
      case Nil => other.parse(s)
      case x => x
    }
  }
}

object Parsez {

  def apply[A](parseFn: S => List[(A, S)]): Parsez[A] = new Parsez[A] {
    def parse(s: S): List[(A, S)] = parseFn(s)
  }

  def runParser[A](parser: Parsez[A], s: List[Char]): Either[String, A] = {
    def parensMatch(text: List[Char], stack: List[Char]): Boolean = (text, stack) match {
      case ('(' :: xs, _) => parensMatch(xs, '(' :: stack)
      case ('[' :: xs, _) => parensMatch(xs, '[' :: stack)
      case (')' :: xs, '(' :: ss) => parensMatch(xs, ss)
      case (')' :: xs, _) => false
      case (']' :: xs, '[' :: ss) => parensMatch(xs, ss)
      case (']' :: xs, _) => false
      case (_ :: xs, _) => parensMatch(xs, stack)
      case (Nil, Nil) => true
      case _ => false
    }

    def assimilateParens(text: List[Char]): List[Char] =
      text.map {
        case '[' => '('
        case ']' => ')'
        case c => c
      }

    if (parensMatch(s, Nil)) {
      parser.parse(assimilateParens(s)) match {
        case List((res, Nil)) => Right(res)
        case List((_, rs)) => Left(s"stream left over: ${rs.mkString}")
        case x => Left(s"parser error: $x")
      }
    } else {
      Left("mismatched parens")
    }
  }

  val item: Parsez[Char] = Parsez {
    case Nil => Nil
    case c :: cs => List((c, cs))
  }

  implicit val parserMonadInstance: Monad[Parsez] =
    new Monad[Parsez] {
      override def bind[A, B](fa: Parsez[A])(f: A => Parsez[B]): Parsez[B] = Parsez { s =>
        fa.parse(s) >>= { case (a, s2) => f(a).parse(s2) }
      }

      override def point[A](a: => A): Parsez[A] = Parsez(s => List((a, s)))
    }

  implicit def parserMonoidInstance[A](implicit monoid: Monoid[A]): Monoid[Parsez[A]] = new Monoid[Parsez[A]] {
    override def zero: Parsez[A] = Parsez(const(Nil))

    override def append(f1: Parsez[A], f2: => Parsez[A]): Parsez[A] = Parsez { s =>
      f1.parse(s) match {
        case Nil => f2.parse(s)
        case f1Parsed @ List((f1Result, f1Remaining)) =>
          f2.parse(f1Remaining) match {
            case Nil => f1Parsed
            case List((f2Result, f2Remaining)) =>
              List((monoid.append(f1Result, f2Result), f2Remaining))
          }

      }
    }
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

  def optional[A](a: A, p: Parsez[A]): Parsez[A] = p <|> a.point[Parsez]

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
