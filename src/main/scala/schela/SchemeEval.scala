package schela

import scala.collection.mutable
import scalaz._
import Scalaz._

import schela.Repl._
import schela.SchemeKeywords._
import schela.Types._

object SchemeEval {

  def apply(f: LispVal, args: List[LispVal]): ThrowsError[LispVal] = f match {
    case LPrimitiveFunc(func) => func(args)

    case LFunc(params, varargs, body, closure) =>
      if (params.size != args.size && varargs.isEmpty) {
        (NumArgs(params.size, args): LispError).raiseError[ThrowsError, LispVal]
      } else {
        val (givenArgs, givenVarargs) = args.splitAt(params.size)
        val newClosure = bindVars(closure, params.zip(givenArgs))
        val newerClosure = varargs.map(argName => bindVars(newClosure, List((argName, LList(givenVarargs))))).getOrElse(newClosure)
        body.map(eval(_, newerClosure.some)).sequence.map(_.last)
      }

    case wrongType => (TypeMismatch("function", wrongType): LispError).raiseError[ThrowsError, LispVal]
  }

  def trapError(action: ThrowsError[String]): ThrowsError[String] =
    action.handleError(_.shows.point[ThrowsError])

  // action should never be a -\/
  def extractValue[A](action: ThrowsError[A]): A = action match {
    case \/-(v) => v
  }

  def eval(v: LispVal, closure: Option[mutable.Map[String, LispVal]] = None): ThrowsError[LispVal] = v match {
    case LChar(_) =>
      v.point[ThrowsError]

    case LString(_) =>
      v.point[ThrowsError]

    case LNumber(_) =>
      v.point[ThrowsError]

    case LBool(_) =>
      v.point[ThrowsError]

    case LAtom(name) => getVar(name.mkString, closure)

    case LList(List(LAtom(`quote`), x)) =>
      x.point[ThrowsError]

    case LList(List(LAtom(`if`), pred, ifTrue, ifFalse)) =>
      eval(pred, closure) >>= {
        case LBool(true) => eval(ifTrue, closure)
        case LBool(false) => eval(ifFalse, closure)
        case _ => (TypeMismatch("bool", pred): LispError).raiseError[ThrowsError, LispVal]
      }

    case LList(List(LAtom(`cond`))) =>
      LUnit().point[ThrowsError]

    case LList(LAtom(`cond`) :: LList(List(LAtom(`else`), result)) :: _) =>
      eval(result, closure)

    case LList(LAtom(`cond`) :: LList(List(pred, result)) :: rest) =>
      eval(pred, closure) >>= {
        case LBool(true) => eval(result, closure)
        case LBool(false) => eval(LList(LAtom(`cond`) :: rest), closure)
        case _ => (TypeMismatch("bool", pred): LispError).raiseError[ThrowsError, LispVal]
      }

    case LList(List(LAtom(`set!`), LAtom(name), form)) =>
      eval(form, closure) >>= (value => setVar(name.mkString, value, closure))

    case LList(List(LAtom(`define`), LAtom(name), form)) =>
      eval(form, closure) >>= (value => defineVar(name.mkString, value, closure))

    case LList(List(LAtom(`load`), LString(fileName))) =>
      loadFile(fileName)

    case LList(LAtom(`define`) :: LList(LAtom(name) :: params) :: body) =>
      defineVar(name.mkString, LFunc(params.map(_.shows), None, body, closure.getOrElse(mutable.Map())))

    case LList(LAtom(`define`) :: LDottedList(LAtom(name) :: params, varargs) :: body) =>
      defineVar(name.mkString, LFunc(params.map(_.shows), Some(varargs.shows), body, closure.getOrElse(mutable.Map())))

    case LList(LAtom(`lambda`) :: LList(params) :: body) =>
      LFunc(params.map(_.shows), None, body, closure.getOrElse(mutable.Map())).point[ThrowsError]

    case LList(LAtom(`lambda`) :: LDottedList(params, varargs) :: body) =>
     LFunc(params.map(_.shows), Some(varargs.shows), body, closure.getOrElse(mutable.Map())).point[ThrowsError]

    case LList(LAtom(`lambda`) :: (varargs @ LAtom(_)) :: body) =>
      LFunc(List.empty, Some((varargs: LispVal).shows), body, closure.getOrElse(mutable.Map())).point[ThrowsError]

    case LList(f :: args) =>
      for {
        func <- eval(f, closure)
        argValues <- args.map(eval(_, closure)).sequence
        result <- apply(func, argValues)
      } yield result

    case badForm => (BadSpecialForm("Unrecognized special form", badForm): LispError).raiseError[ThrowsError, LispVal]
  }
}
