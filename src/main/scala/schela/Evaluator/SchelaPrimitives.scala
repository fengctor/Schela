package schela

import scalaz.Scalaz._
import scalaz.{Foldable, IList, Monoid, NonEmptyList}
import Types._

object SchelaPrimitives {

  private def isSymbol(v: List[SVal]): ThrowsError[SVal] = v match {
    case List(SAtom(_)) => SBool(true).point[ThrowsError]
    case List(_) => SBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  private def isString(v: List[SVal]): ThrowsError[SVal] = v match {
    case List(SString(_)) => SBool(true).point[ThrowsError]
    case List(_) => SBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  private def isNumber(v: List[SVal]): ThrowsError[SVal] = v match {
    case List(SNumber(_)) => SBool(true).point[ThrowsError]
    case List(_) => SBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  private def isList(v: List[SVal]): ThrowsError[SVal] = v match {
    case List(SList(_)) => SBool(true).point[ThrowsError]
    case List(SDottedList(_, _)) => SBool(true).point[ThrowsError]
    case List(_) => SBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  def unwrapNum(v: SVal): ThrowsError[Int] = v match {
    case SNumber(n) => n.point[ThrowsError]
    case notNum => TypeMismatch("number", notNum).raiseError
  }
  def unwrapString(v: SVal): ThrowsError[S] = v match {
    case SString(s) => s.point[ThrowsError]
    case notString => TypeMismatch("string", notString).raiseError
  }
  def unwrapBool(v: SVal): ThrowsError[Boolean] = v match {
    case SBool(b) => b.point[ThrowsError]
    case notBool => TypeMismatch("bool", notBool).raiseError
  }

  def numBinop(op: (Int, Int) => Int)(args: List[SVal]): ThrowsError[SVal] = args match {
    case arg1 :: arg2 :: rest =>
      args.map(unwrapNum).sequence >>= (as => SNumber(as.foldl1Opt(op.curried).get).point[ThrowsError])

    case _ => NumArgs(2, args).raiseError
  }

  def boolBinop[A](
    unwrapper: SVal => ThrowsError[A],
    op: (A, A) => Boolean
  )(args: List[SVal]): ThrowsError[SVal] = args match {
    case List(arg1, arg2) => for {
      l <- unwrapper(arg1)
      r <- unwrapper(arg2)
    } yield SBool(op(l, r))
  }

  private def car(list: List[SVal]): ThrowsError[SVal] = list match {
    case List(SList(x :: _)) => x.point[ThrowsError]
    case List(SDottedList(x :: _, _)) => x.point[ThrowsError]
    case List(invalid) => TypeMismatch("pair", invalid).raiseError
    case tooManyArgs => NumArgs(1, tooManyArgs).raiseError
  }

  private def cdr(list: List[SVal]): ThrowsError[SVal] = list match {
    case List(SList(_ :: xs)) => SList(xs).point[ThrowsError]
    case List(SDottedList(List(_), x)) => x.point[ThrowsError]
    case List(SDottedList(_ :: xs, x)) => SDottedList(xs, x).point[ThrowsError]
    case List(invalid) => TypeMismatch("pair", invalid).raiseError
    case tooManyArgs => NumArgs(1, tooManyArgs).raiseError
  }

  private def cons(args: List[SVal]): ThrowsError[SVal] = args match {
    case List(x, SList(xs)) => SList(x :: xs).point[ThrowsError]
    case List(x, y) => SDottedList(List(x), y).point[ThrowsError]
    case argsMismatch => NumArgs(2, argsMismatch).raiseError
  }

  private def strAppend(args: List[SVal]): ThrowsError[SVal] = args match {
    case str1 :: str2 :: rest =>
      IList.fromList(args)  // compiler thinks I want to use Iterable.fold if using a normal list...
        .traverse {
          case SString(s) => s.point[ThrowsError]
          case invalid => TypeMismatch("string", invalid).raiseError
        }
        .map(strs => SString(strs.fold))

    case _ => NumArgs(2, args).raiseError
  }

  private def eqv(args: List[SVal]): ThrowsError[SVal] = args match {
    case List(SAtom(a1), SAtom(a2)) =>
      SBool(a1 === a2).point[ThrowsError]

    case List(SNumber(n1), SNumber(n2)) =>
      SBool(n1 === n2).point[ThrowsError]

    case List(SChar(c1), SChar(c2)) =>
      SBool(c1 === c2).point[ThrowsError]

    case List(SString(s1), SString(s2)) =>
      SBool(s1 === s2).point[ThrowsError]

    case List(SBool(b1), SBool(b2)) =>
      SBool(b1 === b2).point[ThrowsError]

    case List(SList(Nil), SList(Nil)) =>
      SBool(true).point[ThrowsError]

    case List(SList(x :: xs), SList(y :: ys)) => eqv(List(x, y)) match {
      case Left(_) => SBool(false).point[ThrowsError]
      case Right(SBool(true)) => eqv(List(SList(xs), SList(ys)))
      case Right(SBool(false)) => SBool(false).point[ThrowsError]
    }

    case List(SDottedList(xs, x), SDottedList(ys, y)) =>
      eqv(List(SList(x :: xs), SList(y :: ys)))

    case List(_, _) =>
      SBool(false).point[ThrowsError]

    case wrongNumArgs =>
      NumArgs(2, wrongNumArgs).raiseError
  }

  val primitives: Map[String, List[SVal] => ThrowsError[SVal]] = Map(
    // arithmetic ops
    "+"         -> numBinop(_ + _),
    "-"         -> numBinop(_ - _),
    "*"         -> numBinop(_ * _),
    "/"         -> numBinop(_ / _), // make behaviour like haskell div later
    "mod"       -> numBinop((x, n) => (x % n + n) % n),
    "quotient"  -> numBinop(_ / _),
    "remainder" -> numBinop(_ % _),
    // type-testing ops
    "symbol?" -> isSymbol,
    "string?" -> isString,
    "number?" -> isNumber,
    "list?"   -> isList,
    // predicates
    "="  -> boolBinop[Int](unwrapNum, _ == _),
    "/=" -> boolBinop[Int](unwrapNum, _ != _),
    "<"  -> boolBinop[Int](unwrapNum, _ < _),
    ">"  -> boolBinop[Int](unwrapNum, _ > _),
    "<=" -> boolBinop[Int](unwrapNum, _ <= _),
    ">=" -> boolBinop[Int](unwrapNum, _ >= _),
    "&&" -> boolBinop[Boolean](unwrapBool, _ && _),
    "||" -> boolBinop[Boolean](unwrapBool, _ || _),
    "string=?"  -> boolBinop[S](unwrapString, _ == _),
    "string<?"  -> boolBinop[S](unwrapString, _.mkString < _.mkString),
    "string>?"  -> boolBinop[S](unwrapString, _.mkString > _.mkString),
    "string<=?" -> boolBinop[S](unwrapString, _.mkString <= _.mkString),
    "string>=?" -> boolBinop[S](unwrapString, _.mkString >= _.mkString),
    "eq?"       -> eqv,
    "eqv?"      -> eqv,
    //"equal?" -> ???, // unsure how to implement
    // list primitives
    "car"  -> car,
    "cdr"  -> cdr,
    "cons" -> cons,
    // string primitives
    "str-append" -> strAppend
  )
}
