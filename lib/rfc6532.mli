open Parser

type err += Empty_string

val str   : (char -> bool) -> string t
val dtext : (char -> bool) -> string t
