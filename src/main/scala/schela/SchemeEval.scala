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

    case fun@LFunc(optName, params, varargs, body, env) =>
      if (params.size != args.size && varargs.isEmpty) {
        NumArgs(params.size, args).raiseError
      } else {
        val (givenArgs, givenVarargs) = args.splitAt(params.size)
        val envWithArgs: List[(String, LispVal)] = optName match {
          case None => bindVars (env, params.zip (givenArgs) )
          case Some(name) => bindVars (env, (name -> fun) :: params.zip (givenArgs) )
        }
        val envWithVarargs = varargs
          .map(argName => bindVars(envWithArgs, List((argName, LList(givenVarargs)))))
          .getOrElse(envWithArgs)

        val base: ThrowsError[(LispVal, List[(String, LispVal)])] = (LUnit(), envWithVarargs).point[ThrowsError]
        body.foldLeft(base) { (acc, cur) =>
          acc >>= {
            case (_, newEnv) => eval(cur, newEnv)
          }
        }.map(_._1)
      }

    case wrongType => TypeMismatch("function", wrongType).raiseError
  }

  def trapError(action: ThrowsError[String]): ThrowsError[String] =
    action.handleError(_.shows.point[ThrowsError])

  // action should never be a Left
  def extractValue[A](action: ThrowsError[A]): A = action match {
    case Right(v) => v
  }

  def eval(v: LispVal, env: List[(String, LispVal)]): ThrowsError[(LispVal, List[(String,LispVal)])] = v match {
    case LChar(_) =>
      (v, env).point[ThrowsError]

    case LString(_) =>
      (v, env).point[ThrowsError]

    case LNumber(_) =>
      (v, env).point[ThrowsError]

    case LBool(_) =>
      (v, env).point[ThrowsError]

    case LAtom(name) =>
      getVar(name.mkString, env).map((_, env))

    case LList(List(LAtom(`quote`), x)) =>
      (x, env).point[ThrowsError]

    case LList(List(LAtom(`if`), pred, ifTrue, ifFalse)) =>
      eval(pred, env) >>= {
        case (LBool(true), resEnv) => eval(ifTrue, resEnv)
        case (LBool(false), resEnv) => eval(ifFalse, resEnv)
        case (_, resEnv) => TypeMismatch("bool", pred).raiseError
      }

    case LList(List(LAtom(`cond`))) =>
      (LUnit(), env).point[ThrowsError]

    case LList(LAtom(`cond`) :: LList(List(LAtom(`else`), result)) :: _) =>
      eval(result, env)

    case LList(LAtom(`cond`) :: LList(List(pred, result)) :: rest) =>
      eval(pred, env) >>= {
        case (LBool(true), resEnv) => eval(result, resEnv)
        case (LBool(false), resEnv) => eval(LList(LAtom(`cond`) :: rest), resEnv)
        case _ => TypeMismatch("bool", pred).raiseError
      }

    /* byebye impure
    case LList(List(LAtom(`set!`), LAtom(name), form)) =>
      eval(form, closure) >>= (value => setVar(name.mkString, value, closure))*/

    case LList(List(LAtom(`load`), LString(fileName))) =>
      loadFile(fileName, env)

    case LList(List(LAtom(`let`), LList(Nil), body)) =>
      eval(body, env)

    case LList(List(LAtom(`let`), LList(LList(List(LAtom(name), form)) :: otherBindings), body)) =>
      eval(form, env) >>= {
        case (value, newEnv) => eval(
          LList(List(LAtom(`let`), LList(otherBindings), body)),
          defineVar(name.mkString, value, newEnv)
        )
      }

    case LList(List(LAtom(`define`), LAtom(name), form)) =>
      eval(form, env).map {
        case (value, newEnv) => (LUnit(), defineVar(name.mkString, value, newEnv))
      }

    case LList(LAtom(`define`) :: LList(LAtom(name) :: params) :: body) =>
      val fName = name.mkString
      (LUnit(), defineVar(fName, LFunc(fName.some, params.map(_.shows), None, body, env), env)).point[ThrowsError]

    case LList(LAtom(`define`) :: LDottedList(LAtom(name) :: params, varargs) :: body) =>
      val fName = name.mkString
      (LUnit(), defineVar(fName, LFunc(fName.some, params.map(_.shows), Some(varargs.shows), body, env), env)).point[ThrowsError]

    case LList(LAtom(`lambda`) :: LList(params) :: body) =>
      (LFunc(none, params.map(_.shows), None, body, env), env).point[ThrowsError]

    case LList(LAtom(`lambda`) :: LDottedList(params, varargs) :: body) =>
      (LFunc(none, params.map(_.shows), Some(varargs.shows), body, env), env).point[ThrowsError]

    case LList(LAtom(`lambda`) :: (varargs @ LAtom(_)) :: body) =>
      (LFunc(none, List.empty, Some((varargs: LispVal).shows), body, env), env).point[ThrowsError]

    case LList(f :: args) =>
      for {
        (func, newEnv) <- eval(f, env)
        argValues <- args.map(eval(_, newEnv).map(_._1)).sequence
        result <- apply(func, argValues)
      } yield (result, env)

    case badForm => BadSpecialForm("Unrecognized special form", badForm).raiseError
  }
}
