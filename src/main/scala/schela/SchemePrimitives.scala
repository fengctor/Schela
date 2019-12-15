package schela

import scalaz._
import Scalaz._

import schela.SchemeKeywords._
import schela.Types._

object SchemePrimitives {

  private def isSymbol(v: List[LispVal]): ThrowsError[LispVal] = v match {
    case List(LAtom(_)) => LBool(true).point[ThrowsError]
    case List(_) => LBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  private def isString(v: List[LispVal]): ThrowsError[LispVal] = v match {
    case List(LString(_)) => LBool(true).point[ThrowsError]
    case List(_) => LBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  private def isNumber(v: List[LispVal]): ThrowsError[LispVal] = v match {
    case List(LNumber(_)) => LBool(true).point[ThrowsError]
    case List(_) => LBool(false).point[ThrowsError]
    case given => NumArgs(1, given).raiseError
  }

  def unwrapNum(v: LispVal): ThrowsError[Int] = v match {
    case LNumber(n) => n.point[ThrowsError]
    case notNum => TypeMismatch("number", notNum).raiseError
  }
  def unwrapString(v: LispVal): ThrowsError[List[Char]] = v match {
    case LString(s) => s.point[ThrowsError]
    case notString => TypeMismatch("string", notString).raiseError
  }
  def unwrapBool(v: LispVal): ThrowsError[Boolean] = v match {
    case LBool(b) => b.point[ThrowsError]
    case notBool => TypeMismatch("bool", notBool).raiseError
  }

  def numBinop(op: (Int, Int) => Int)(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case arg1 :: arg2 :: rest =>
      args.map(unwrapNum).sequence >>= (as => LNumber(as.foldl1Opt(op.curried).get).point[ThrowsError])

    case _ => NumArgs(2, args).raiseError
  }

  def boolBinop[A](
    unwrapper: LispVal => ThrowsError[A],
    op: (A, A) => Boolean
  )(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case List(arg1, arg2) => for {
      l <- unwrapper(arg1)
      r <- unwrapper(arg2)
    } yield LBool(op(l, r))
  }

  private def car(list: List[LispVal]): ThrowsError[LispVal] = list match {
    case List(LList(x :: _)) => x.point[ThrowsError]
    case List(LDottedList(x :: _, _)) => x.point[ThrowsError]
    case List(invalid) => TypeMismatch("pair", invalid).raiseError
    case tooManyArgs => NumArgs(1, tooManyArgs).raiseError
  }

  private def cdr(list: List[LispVal]): ThrowsError[LispVal] = list match {
    case List(LList(_ :: xs)) => LList(xs).point[ThrowsError]
    case List(LDottedList(List(_), x)) => x.point[ThrowsError]
    case List(LDottedList(_ :: xs, x)) => LDottedList(xs, x).point[ThrowsError]
    case List(invalid) => TypeMismatch("pair", invalid).raiseError
    case tooManyArgs => NumArgs(1, tooManyArgs).raiseError
  }

  private def cons(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case List(x, LList(xs)) => LList(x :: xs).point[ThrowsError]
    case List(x, y) => LDottedList(List(x), y).point[ThrowsError]
    case argsMismatch => NumArgs(2, argsMismatch).raiseError
  }

  private def eqv(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case List(LAtom(a1), LAtom(a2)) =>
      LBool(a1 == a2).point[ThrowsError]

    case List(LNumber(n1), LNumber(n2)) =>
      LBool(n1 == n2).point[ThrowsError]

    case List(LChar(c1), LChar(c2)) =>
      LBool(c1 == c2).point[ThrowsError]

    case List(LString(s1), LString(s2)) =>
      LBool(s1 == s2).point[ThrowsError]

    case List(LBool(b1), LBool(b2)) =>
      LBool(b1 == b2).point[ThrowsError]

    case List(LList(Nil), LList(Nil)) =>
      LBool(true).point[ThrowsError]

    case List(LList(x :: xs), LList(y :: ys)) => eqv(List(x, y)) match {
      case Left(_) => LBool(false).point[ThrowsError]
      case Right(LBool(true)) => eqv(List(LList(xs), LList(ys)))
      case Right(LBool(false)) => LBool(false).point[ThrowsError]
    }

    case List(LDottedList(xs, x), LDottedList(ys, y)) =>
      eqv(List(LList(x :: xs), LList(y :: ys)))

    case List(_, _) =>
      LBool(false).point[ThrowsError]

    case wrongNumArgs =>
      NumArgs(2, wrongNumArgs).raiseError
  }

  val primitives: List[(String, List[LispVal] => ThrowsError[LispVal])] = List(
    // arithmetic ops
    "+" -> numBinop(_ + _),
    "-" -> numBinop(_ - _),
    "*" -> numBinop(_ * _),
    "/" -> numBinop(_ / _), // make behaviour like haskell div later
    "mod" -> numBinop((x, n) => (x % n + n) % n),
    "quotient" -> numBinop(_ / _),
    "remainder" -> numBinop(_ % _),
    // type-testing ops
    "symbol?" -> isSymbol,
    "string?" -> isString,
    "number?" -> isNumber,
    // predicates
    "=" -> boolBinop[Int](unwrapNum, _ == _),
    "/=" -> boolBinop[Int](unwrapNum, _ != _),
    "<" -> boolBinop[Int](unwrapNum, _ < _),
    ">" -> boolBinop[Int](unwrapNum, _ > _),
    "<=" -> boolBinop[Int](unwrapNum, _ <= _),
    ">=" -> boolBinop[Int](unwrapNum, _ >= _),
    "&&" -> boolBinop[Boolean](unwrapBool, _ && _),
    "||" -> boolBinop[Boolean](unwrapBool, _ || _),
    "string=?" -> boolBinop[List[Char]](unwrapString, _ == _),
    "string<?" -> boolBinop[List[Char]](unwrapString, _ < _),
    "string>?" -> boolBinop[List[Char]](unwrapString, _ > _),
    "string<=?" -> boolBinop[List[Char]](unwrapString, _ <= _),
    "string>=?" -> boolBinop[List[Char]](unwrapString, _ >= _),
    "eq?" -> eqv,
    "eqv?" -> eqv,
    //"equal?" -> ???, // unsure how to implement
    // list primitives
    "car" -> car,
    "cdr" -> cdr,
    "cons" -> cons
  )
}
