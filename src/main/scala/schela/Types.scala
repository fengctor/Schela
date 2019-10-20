package schela

import scalaz._

object Types {
  type S = List[Char]
  type ThrowsError[+A] = Either[LispError, A]
}
