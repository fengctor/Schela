package schela

import java.io.FileNotFoundException

import scala.io.BufferedSource

import scalaz._
import Scalaz._

import Parsez._
import SchelaParse._
import SchelaPrimitives._
import SchelaKeywords._
import Types._

object SchelaEval {

  implicit val CharEqual: Equal[Char] = _ == _ // Not sure where this is in Scalaz 🤔

  val prims: Env = Env(
    primitives.map { case (v, func) => (v, SPrimitiveFunc(func)) },
    Map.empty
  )

  // patterns
  val `consPat`: List[Char] = "cons".toList
  val `listPat`: List[Char] = "list".toList

  def getVar(name: String, env: Env): ThrowsError[SVal] = env.getBinding(name)

  def defineVar(name: String, value: SVal, env: Env): ThrowsError[Env] = {
    if (allKeywords.contains(name.toList)) {
      KeywordShadowing(name).raiseError
    } else {
      env.withBinding(name, value).point[ThrowsError]
    }
  }

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
      patList.zip(exprList).foldLeft(env.some) {
        case (accEnv, (curPat, curExpr)) => accEnv >>= (matchAttempt(curPat, curExpr, _))
      }
    case (quoted@SList(SAtom(`quote`) :: _), _) =>
      eval(quoted, env).toOption >>= {
        case (evalPat, evalEnv) => literalMatchAttempt(evalPat, expr, evalEnv)
      }

    // struct patterns
    case (SList(SAtom(possibleStruct) :: argsPat), SStruct(name, args))
      if argsPat.size === args.size && possibleStruct === name =>
      argsPat.zip(args).foldLeft(env.some) {
        case (accEnv, (curPat, curExpr)) => accEnv >>= (matchAttempt(curPat, curExpr, _))
      }

    // literal patterns
    case (_, _) => literalMatchAttempt(pattern, expr, env)
  }

  def fileParseAttempt(fileStr: String, parser: Parsez[List[SVal]], input: S, env: Env): ThrowsError[(SVal, Env)] = {
    val commentsRemoved = deleteComments(input)

    if (parensMatch(commentsRemoved)) {
      runParser(parser, roundifyParens(commentsRemoved)) match {
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

  def evalCond(clauses: List[SVal], env: Env): ThrowsError[SVal] = clauses match {
    case Nil =>
      SUnit().point[ThrowsError]

    case SList(List(SAtom(`else`), result)) :: _ =>
      eval(result, env).map(_._1)

    case SList(List(pred, result)) :: restClauses =>
      eval(pred, env) >>= {
        case (SBool(true), resEnv) => eval(result, resEnv).map(_._1)
        case (SBool(false), resEnv) => evalCond(restClauses, resEnv)
        case _ => TypeMismatch("bool", pred).raiseError
      }
    case badForm :: _ => BadSpecialForm("Malformed cond clause", badForm).raiseError
  }

  // expr must already be evaluated
  def evalMatch(expr: SVal, clauses: List[SVal], env: Env): ThrowsError[SVal] = clauses match {
    case Nil => MatchFailure("No match found", expr).raiseError
    case SList(List(pattern, result)) :: rest => matchAttempt(pattern, expr, env) match {
      case None => evalMatch(expr, rest, env)
      case Some(matchEnv) => eval(result, matchEnv).map(_._1)
    }
    case badForm :: _ => BadSpecialForm("Malformed match clause", badForm).raiseError
  }

  def evalLet(bindings: List[SVal], body: SVal, env: Env): ThrowsError[SVal] = bindings match {
    case Nil => eval(body, env).map(_._1)
    case SList(List(SAtom(name), form)) :: otherBindings =>
      for {
        (value, newEnv) <- eval(form, env)
        boundVarEnv     <- defineVar(name.mkString, value, newEnv)
        result          <- evalLet(otherBindings, body, boundVarEnv)
      } yield result
    case badClause :: _ => BadSpecialForm("Malformed let clause", badClause).raiseError
  }

  def validateParams(params: List[SVal]): ThrowsError[List[SAtom]] =
    params.traverse {
      case atom@SAtom(_) => atom.point[ThrowsError]
      case badParam => BadSpecialForm("Invalid param", badParam).raiseError
    }

  def attemptStructConstruction(s: SVal, args: List[SVal], env: Env): ThrowsError[SVal] = s match {
    case SAtom(name) =>
      val nameStr = name.mkString
      for {
        numParams <- env.getStruct(nameStr)
        evalArgs  <- args.map(eval(_, env).map(_._1)).sequence
        result    <-
          if (numParams === args.size) {
            SStruct(nameStr, evalArgs).point[ThrowsError]
          } else {
            NumArgs(numParams, evalArgs).raiseError
          }
      } yield result

    case _ => BadSpecialForm("Not a struct", s).raiseError
  }

  def attemptFunctionApplication(f: SVal, args: List[SVal], env: Env): ThrowsError[(SVal, Env)] =
    for {
      (func, newEnv) <- eval(f, env)
      argValues      <- args.map(eval(_, newEnv).map(_._1)).sequence
      result         <- apply(func, argValues)
    } yield (result, env)

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
      evalCond(clauses, env).map((_, env))

    case SList(SAtom(`match`) :: expr :: clauses) =>
      eval(expr, env) >>= {
        case (evalExpr, newEnv) => evalMatch(evalExpr, clauses, newEnv).map((_, newEnv))
      }

    case SList(List(SAtom(`let`), SList(bindings), body)) =>
      evalLet(bindings, body, env).map((_, env))

    case SList(List(SAtom(`load`), SString(fileName))) =>
      loadFile(fileName, env)

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

    case SList(List(SAtom(`struct`), SAtom(name), SList(givenParams))) =>
      validateParams(givenParams).map { params =>
        (SUnit(), env.withStruct(name.mkString, params.map(_.name.mkString)))
      }

    case SList(SAtom(`lambda`) :: SList(params) :: body) =>
      (SFunc(none, params.map(_.shows), None, body, env), env).point[ThrowsError]

    case SList(SAtom(`lambda`) :: SDottedList(params, varargs) :: body) =>
      (SFunc(none, params.map(_.shows), Some(varargs.shows), body, env), env).point[ThrowsError]

    case SList(SAtom(`lambda`) :: (varargs @ SAtom(_)) :: body) =>
      (SFunc(none, List.empty, Some((varargs: SVal).shows), body, env), env).point[ThrowsError]

    case SList(f :: args) =>
      // TODO: maybe some kind of mixed priority monoid?
      //   (ie: if struct defn exists but wrong number of args, take that error)
      attemptStructConstruction(f, args, env).map((_, env)) |+| attemptFunctionApplication(f, args, env)

    case badForm => BadSpecialForm("Unrecognized special form", badForm).raiseError
  }
}
