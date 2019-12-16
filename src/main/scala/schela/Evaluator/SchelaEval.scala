package schela

import java.io.FileNotFoundException

import scalaz._
import Scalaz._

import Parsez._
import SchelaParse._
import SchelaPrimitives._
import SchelaKeywords._
import Types.{Env, ThrowsError}

import scala.io.BufferedSource

object SchelaEval {

  implicit def CharEqual: Equal[Char] = _ == _ // Not sure where this is in Scalaz 🤔

  val prims: Env =
    primitives.map { case (v, func) => (v, SPrimitiveFunc(func)) }

  def getVar(name: String, env: Env): ThrowsError[SVal] =
    env.find(_._1 === name) match {
      case Some((_, value)) => value.point[ThrowsError]
      case _ => UnboundVar("Getting an unbound variable", name).raiseError
    }

  def defineVar(name: String, value: SVal, env: Env): Env = {
    (name -> value) :: env
  }

  def bindVars(env: Env, vs: Env): Env = vs ++ env

  def matchAttempt(pattern: SVal, expr: SVal, env: Env): Option[Env] = (pattern, expr) match {
    case (SAtom(List('_')), _) => env.some
    case (SAtom(v), _) => ((v.mkString -> expr) :: env).some

    // list patterns
    case (SList(Nil), SList(Nil)) => env.some
    case (SList(List(SAtom(`consPat`), headPat, tailPat)), SList(x :: xs)) =>
      matchAttempt(headPat, x, env) >>= { headEnv =>
        matchAttempt(tailPat, SList(xs), headEnv)
      }
    case (SList(SAtom(`listPat`) :: patList), SList(exprList)) if patList.size === exprList.size =>
      patList.zip(exprList).foldl(env.some) { accEnv =>
        { case (curPat, curExpr) => accEnv >>= (matchAttempt(curPat, curExpr, _)) }
      }

    // literal patterns
    case (SNumber(patN), SNumber(exprN)) if patN === exprN => env.some
    case (SChar(patC), SChar(exprC))     if patC === exprC => env.some
    case (SString(patS), SString(exprS)) if patS === exprS => env.some
    case (SBool(patB), SBool(exprB))     if patB === exprB => env.some

    case _ => none
  }

  def fileParseAttempt(
    fileStr: String,
    parser: Parsez[List[SVal]],
    input: List[Char],
    env: Env
  ): ThrowsError[(SVal, Env)] = {
    runParser(parser, input) match {
      case Left(err) =>
        Parser(err).raiseError
      case Right(Nil) =>
        (SAtom(s"$fileStr loaded, but nothing happened".toList), env).point[ThrowsError]
      case Right(v :: vs) =>
        vs.foldLeft(eval(v, env)) { (acc, cur) =>
          acc >>= {
            case (_, resEnv) => eval(cur, resEnv)
          }
        } match {
          case Left(err) =>
            err.raiseError
          case Right((_, resEnv)) =>
            (SAtom(s"$fileStr loaded".toList), resEnv).point[ThrowsError]
        }
    }
  }
  def loadFile(fileName: List[Char], env: Env): ThrowsError[(SVal, Env)] = {
    def fromFileOpt(file: String): Option[BufferedSource] = {
      try {
        val src = scala.io.Source.fromFile(file)
        src.some
      } catch {
        case _: FileNotFoundException => none
      }
    }

    val fileStr = fileName.mkString
    val parser: Parsez[List[SVal]] = endBy(parseExpr, spaces)
    val srcOpt = fromFileOpt(fileStr)
    val inputOpt = srcOpt.map(_.toList)

    srcOpt.foreach(_.close())

    inputOpt match {
      case None => FileNotFound(fileStr).raiseError
      case Some(input) => fileParseAttempt(fileStr, parser, input, env)
    }
  }

  def apply(f: SVal, args: List[SVal]): ThrowsError[SVal] = f match {
    case SPrimitiveFunc(func) => func(args)

    case fun@SFunc(optName, params, varargs, body, env) =>
      if (params.size != args.size && varargs.isEmpty) {
        NumArgs(params.size, args).raiseError
      } else {
        val (givenArgs, givenVarargs) = args.splitAt(params.size)
        val envWithArgs: Env = optName.fold(identity[Env] _) { name =>
          argEnv => bindVars(argEnv, List(name -> fun))
        } (bindVars(env, params.zip (givenArgs)))
        val envWithVarargs = varargs
          .map(argName => bindVars(envWithArgs, List((argName, SList(givenVarargs)))))
          .getOrElse(envWithArgs)

        val base: ThrowsError[(SVal, Env)] = (SUnit(), envWithVarargs).point[ThrowsError]
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

  // expr must already be evaluated
  def evalMatch(expr: SVal, clauses: List[SVal], env: Env): ThrowsError[(SVal, Env)] = clauses match {
    case Nil => MatchFailure("No match found", expr).raiseError
    case SList(List(pattern, result)) :: rest => matchAttempt(pattern, expr, env) match {
      case None => evalMatch(expr, rest, env)
      case Some(matchEnv) => eval(result, matchEnv)
    }
    case badForm :: _ => BadSpecialForm("Invalid match clause", badForm).raiseError
  }

  def eval(v: SVal, env: Env): ThrowsError[(SVal, Env)] = v match {
    case SChar(_) =>
      (v, env).point[ThrowsError]

    case SString(_) =>
      (v, env).point[ThrowsError]

    case SNumber(_) =>
      (v, env).point[ThrowsError]

    case SBool(_) =>
      (v, env).point[ThrowsError]

    case SAtom(name) =>
      getVar(name.mkString, env).map((_, env))

    case SList(List(SAtom(`quote`), x: SVal)) =>
      (x, env).point[ThrowsError]

    case SList(List(SAtom(`if`), pred, ifTrue, ifFalse)) =>
      eval(pred, env) >>= {
        case (SBool(true), resEnv) => eval(ifTrue, resEnv)
        case (SBool(false), resEnv) => eval(ifFalse, resEnv)
        case (_, resEnv) => TypeMismatch("bool", pred).raiseError
      }

    case SList(List(SAtom(`cond`))) =>
      (SUnit(), env).point[ThrowsError]

    case SList(SAtom(`cond`) :: SList(List(SAtom(`else`), result)) :: _) =>
      eval(result, env)

    case SList(SAtom(`cond`) ::
      SList(List(pred, result)) ::
      rest
    ) =>
      eval(pred, env) >>= {
        case (SBool(true), resEnv) => eval(result, resEnv)
        case (SBool(false), resEnv) => eval(SList(SAtom(`cond`) :: rest), resEnv)
        case _ => TypeMismatch("bool", pred).raiseError
      }

    case SList(SAtom(`match`) :: expr :: clauses) =>
      eval(expr, env) >>= { case (evalExpr, newEnv) => evalMatch(evalExpr, clauses, newEnv) }

    case SList(List(SAtom(`load`), SString(fileName))) =>
      loadFile(fileName, env)

    case SList(List(SAtom(`let`), SList(Nil), body)) =>
      eval(body, env)

    case SList(List(SAtom(`let`), SList(SList(List(SAtom(name), form)) :: otherBindings), body)) =>
      eval(form, env) >>= {
        case (value, newEnv) => eval(
          SList(List(SAtom(`let`), SList(otherBindings), body)),
          defineVar(name.mkString, value, newEnv)
        )
      }

    case SList(List(SAtom(`define`), SAtom(name), form)) =>
      eval(form, env).map {
        case (value, newEnv) => (SUnit(), defineVar(name.mkString, value, newEnv))
      }

    case SList(SAtom(`define`) :: SList(SAtom(name) :: params) :: body) =>
      val fName = name.mkString
      (
        SUnit(),
        defineVar(fName, SFunc(fName.some, params.map(_.shows), None, body, env), env)
      ).point[ThrowsError]

    case SList(SAtom(`define`) :: SDottedList(SAtom(name) :: params, varargs) :: body) =>
      val fName = name.mkString
      (
        SUnit(),
        defineVar(fName, SFunc(fName.some, params.map(_.shows), Some(varargs.shows), body, env), env)
      ).point[ThrowsError]

    case SList(SAtom(`lambda`) :: SList(params) :: body) =>
      (SFunc(none, params.map(_.shows), None, body, env), env).point[ThrowsError]

    case SList(SAtom(`lambda`) :: SDottedList(params, varargs) :: body) =>
      (SFunc(none, params.map(_.shows), Some(varargs.shows), body, env), env).point[ThrowsError]

    case SList(SAtom(`lambda`) :: (varargs @ SAtom(_)) :: body) =>
      (SFunc(none, List.empty, Some((varargs: SVal).shows), body, env), env).point[ThrowsError]

    case SList(f :: args) =>
      for {
        (func, newEnv) <- eval(f, env)
        argValues <- args.map(eval(_, newEnv).map(_._1)).sequence
        result <- apply(func, argValues)
      } yield (result, env)

    case badForm => BadSpecialForm("Unrecognized special form", badForm).raiseError
  }
}
