package schela

object SchelaKeywords {
  val quote: List[Char] = "quote".toList
  val `if`: List[Char] = "if".toList
  val cond: List[Char] = "cond".toList
  val `else`: List[Char] = "else".toList // what if I define else in the stdlib to be #t?
  val `match`: List[Char] = "match".toList
  val `consPat`: List[Char] = "cons".toList
  val `listPat`: List[Char] = "list".toList
  val let: List[Char] = "let".toList
  val define: List[Char] = "define".toList
  val lambda: List[Char] = "lambda".toList
  val load: List[Char] = "load".toList
}