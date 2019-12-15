package schela

object Types {
  type S = List[Char]
  type ThrowsError[+A] = Either[SError, A]
  type Env = List[(String, SVal)]
}
