package schela

import java.io.FileNotFoundException

import scalaz._
import Scalaz._
import Parsez._
import SchelaParse._
import SchelaPrimitives._
import SchelaKeywords._
import Types._

import scala.io.BufferedSource

object SchelaEval {

  implicit val CharEqual: Equal[Char] = _ == _ // Not sure where this is in Scalaz 🤔

  val prims: Env =
    primitives.map { case (v, func) => (v, SPrimitiveFunc(func)) }

  def getVar(name: String, env: Env): ThrowsError[SVal] =
    env.get(name) match {
      case None        => UnboundVar("Getting an unbound variable", name).raiseError
      case Some(value) => value.point[ThrowsError]
    }

  def defineVar(name: String, value: SVal, env: Env): ThrowsError[Env] = {
    env.get(name) match {
      case Some(f@SFunc(_, _, _, _, _)) => BadSpecialForm(s"$name is already a defined function", f).raiseError // TODO: new error type, only do this for functions
      case _                            => (env + (name -> value)).point[ThrowsError]
    }
  }

  // TODO: check duplicates
  def bindVars(vs: List[(String, SVal)], env: Env): ThrowsError[Env] =
    vs.foldLeft(env.point[ThrowsError]) { (envResult, c) =>
      val (name, value) = c
      envResult >>= (e => defineVar(name, value, e))
    }

  private def literalMatchAttempt(patExpr: SVal, expr: SVal, env: Env): Option[Env] = (patExpr, expr) match {
    case (SAtom(patA), SAtom(exprA))     if patA === exprA => env.some
    case (SNumber(patN), SNumber(exprN)) if patN === exprN => env.some
    case (SChar(patC), SChar(exprC))     if patC === exprC => env.some
    case (SString(patS), SString(exprS)) if patS === exprS => env.some
    case (SBool(patB), SBool(exprB))     if patB === exprB => env.some
    case (SList(patL), SList(exprL))     if patL === exprL => env.some
    case _ => none
  }

  private def matchAttempt(pattern: SVal, expr: SVal, env: Env): Option[Env] = (pattern, expr) match {
    case (SAtom(List('_')), _) => env.some
    case (SAtom(v), _) => defineVar(v.mkString, expr, env).toOption

    // list patterns
    case (SList(Nil), SList(Nil)) => env.some
    case (SList(List(SAtom(`consPat`), headPat, tailPat)), SList(x :: xs)) =>
      matchAttempt(headPat, x, env) >>= (matchAttempt(tailPat, SList(xs), _))
    case (SList(SAtom(`listPat`) :: patList), SList(exprList)) if patList.size === exprList.size =>
      patList.zip(exprList).foldl(env.some) { accEnv =>
        { case (curPat, curExpr) => accEnv >>= (matchAttempt(curPat, curExpr, _)) }
      }
    case (quoted@SList(SAtom(`quote`) :: _), _) =>
      eval(quoted, env).toOption >>= { case (evalPat, evalEnv) => literalMatchAttempt(evalPat, expr, evalEnv) }

    // literal patterns
    case (_, _) => literalMatchAttempt(pattern, expr, env)
  }

  def fileParseAttempt(
    fileStr: String,
    parser: Parsez[List[SVal]],
    input: S,
    env: Env
  ): ThrowsError[(SVal, Env)] = {
    val commentsRemoved = deleteComments(input)

    if (parensMatch(commentsRemoved, Nil)) {
      runParser(parser, assimilateParens(commentsRemoved)) match {
        case Left(err) =>
          Parser(err).raiseError
        case Right(Nil) =>
          (SAtom(s"$fileStr loaded, but nothing happened".toList), env).point[ThrowsError]
        case Right(v :: vs) =>
          vs.foldLeft(eval(v, env)) { (acc, cur) =>
            acc >>= { case (_, resEnv) => eval(cur, resEnv) }
          } match {
            case Left(err) =>
              err.raiseError
            case Right((_, resEnv)) =>
              (SAtom(s"$fileStr loaded".toList), resEnv).point[ThrowsError]
          }
      }
    } else {
      Parser("mismatched parens").raiseError
    }
  }

  def loadFile(fileName: S, env: Env): ThrowsError[(SVal, Env)] = {
    def fromFileOpt(file: String): Option[BufferedSource] = {
      try {
        val src = scala.io.Source.fromFile(file)
        src.some
      } catch {
        case _: FileNotFoundException => none
      }
    }

    val fileStr = fileName.mkString
    val parser: Parsez[List[SVal]] = spaces *> endBy(parseExpr, spaces1)
    val srcOpt = fromFileOpt(fileStr)
    val inputOpt = srcOpt.map(_.toSeq)

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
        val envWithArgs: ThrowsError[Env] = optName.fold(identity[ThrowsError[Env]] _) { name =>
          argEnv => argEnv >>= (defineVar(name, fun, _))
        } (bindVars(params.zip(givenArgs), env))
        val envWithVarargs: ThrowsError[Env] = varargs
          .map { argName =>
            envWithArgs >>= (bindVars(List((argName, SList(givenVarargs))), _))
          }
          .getOrElse(envWithArgs)

        val base: ThrowsError[(SVal, Env)] = envWithVarargs.map((SUnit(), _))
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

  def evalCond(clauses: List[SVal], env: Env): ThrowsError[(SVal, Env)] = clauses match {
    case Nil =>
      (SUnit(), env).point[ThrowsError] // TODO: cond error

    case SList(List(SAtom(`else`), result)) :: _ =>
      eval(result, env)

    case SList(List(pred, result)) :: restClauses =>
      eval(pred, env) >>= {
        case (SBool(true), resEnv) => eval(result, resEnv)
        case (SBool(false), resEnv) => evalCond(restClauses, resEnv)
        case _ => TypeMismatch("bool", pred).raiseError
      }
    case badForm :: _ => BadSpecialForm("Invalid cond clause", badForm).raiseError
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

    case SList(SAtom(`cond`) :: clauses) =>
      evalCond(clauses, env)

    case SList(SAtom(`match`) :: expr :: clauses) =>
      eval(expr, env) >>= { case (evalExpr, newEnv) => evalMatch(evalExpr, clauses, newEnv) }

    case SList(List(SAtom(`load`), SString(fileName))) =>
      loadFile(fileName, env)

    case SList(List(SAtom(`let`), SList(Nil), body)) =>
      eval(body, env)

    case SList(List(SAtom(`let`), SList(SList(List(SAtom(name), form)) :: otherBindings), body)) =>
      eval(form, env) >>= {
        case (value, newEnv) => defineVar(name.mkString, value, newEnv) >>= {
          eval(SList(List(SAtom(`let`), SList(otherBindings), body)), _)
        }
      }

    case SList(List(SAtom(`define`), SAtom(name), form)) =>
      for {
        (value, newEnv) <- eval(form, env)
        defEnv          <- defineVar(name.mkString, value, newEnv)
      } yield (SUnit(), defEnv)

    case SList(SAtom(`define`) :: SList(SAtom(name) :: params) :: body) =>
      val fName = name.mkString
      defineVar(fName, SFunc(fName.some, params.map(_.shows), None, body, env), env)
        .map((SUnit(), _))

    case SList(SAtom(`define`) :: SDottedList(SAtom(name) :: params, varargs) :: body) =>
      val fName = name.mkString
      defineVar(fName, SFunc(fName.some, params.map(_.shows), Some(varargs.shows), body, env), env)
        .map((SUnit(), _))

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
