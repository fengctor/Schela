package schela

import scalaz.{Equal, Monoid, Semigroup}

object Types {
  type S = Seq[Char]
  type ThrowsError[+A] = Either[SError, A]
  //type Env = Map[String, SVal]

  final case class Env(bindings: Map[String, SVal], structs: Map[String, Int]) {

    def withBinding(name: String, value: SVal): Env = Env(bindings + (name -> value), structs)

    def getBinding(name: String): Option[SVal] = bindings.get(name)

    def withStruct(name: String, numParams: Int): Env = Env(bindings, structs + (name -> numParams))

    def getStruct(name: String): Option[Int] = structs.get(name)
  }

  implicit def SeqEqual[A](implicit eqA: Equal[A]): Equal[Seq[A]] = Equal.equalA
  implicit def SeqMonoid[A]: Monoid[Seq[A]] = Monoid.instance(_ ++ _, Seq.empty[A])

  // a right-biased Semigroup instance
  implicit def ThrowsErrorSemigroup[A]: Semigroup[ThrowsError[A]] = Semigroup.instance {
    case (fst@Right(_), _) => fst
    case (Left(_), snd) => snd
  }
}
