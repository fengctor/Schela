package schela

import scalaz._
import Scalaz._

object Types {
  type S = Seq[Char]
  type ThrowsError[+A] = Either[SError, A]

  final case class Env(bindings: Map[String, SVal], structs: Map[String, Int]) {

    def withBinding(name: String, value: SVal): Env = Env(bindings + (name -> value), structs)

    def getBinding(name: String): ThrowsError[SVal] = bindings.get(name) match {
      case None        => UnboundVar("Getting an unbound variable", name).raiseError
      case Some(value) => value.point[ThrowsError]
    }

    def withStruct(name: String, params: List[String]): Env = Env(
      bindings ++ params.map { paramName =>
        val getterName = s"$name-$paramName"
        (getterName, SFunc(getterName.some, List("arg"), none,
          List(Parsez.runParser(SchelaParse.parseExpr,
            s"(match arg (($name ${params.mkString(" ")}) $paramName))".toSeq
          ).getOrElse(SUnit())),
          this
        ))
      },
      structs + (name -> params.size)
    )

    def getStruct(name: String): ThrowsError[Int] = structs.get(name) match {
      case None => BadSpecialForm("Not a struct", SAtom(name)).raiseError
      case Some(n) => Right(n)
    }
  }

  implicit def SeqEqual[A](implicit eqA: Equal[A]): Equal[Seq[A]] = Equal.equalA
  implicit def SeqMonoid[A]: Monoid[Seq[A]] = Monoid.instance(_ ++ _, Seq.empty[A])

  // a right-biased Semigroup instance
  implicit def ThrowsErrorSemigroup[A]: Semigroup[ThrowsError[A]] = Semigroup.instance {
    case (fst@Right(_), _) => fst
    case (Left(_), snd) => snd
  }
}
