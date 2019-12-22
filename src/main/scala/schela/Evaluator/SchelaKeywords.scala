package schela

object SchelaKeywords {
  val `quote`: List[Char] = "quote".toList
  val `if`: List[Char] = "if".toList
  val `cond`: List[Char] = "cond".toList
  val `else`: List[Char] = "else".toList
  val `match`: List[Char] = "match".toList
  val `let`: List[Char] = "let".toList
  val `define`: List[Char] = "define".toList
  val `struct`: List[Char] = "struct".toList
  val `lambda`: List[Char] = "lambda".toList
  val `load`: List[Char] = "load".toList

  val allKeywords: Set[List[Char]] = Set(
    `quote`,
    `if`,
    `cond`,
    `else`,
    `match`,
    `let`,
    `define`,
    `struct`,
    `lambda`,
    `load`
  )
}
