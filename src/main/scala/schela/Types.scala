package schela

import scalaz.{Equal, Monoid}

object Types {
  type S = Seq[Char]
  type ThrowsError[+A] = Either[SError, A]
  type Env = List[(String, SVal)]

  implicit def SeqEqual[A](implicit eqA: Equal[A]): Equal[Seq[A]] = Equal.equalA
  implicit def SeqMonoid[A]: Monoid[Seq[A]] = Monoid.instance(_ ++ _, Seq.empty[A])
}
