import java.io.StringReader

import homework1.{Lexer, Parser}

val in = new Lexer(new StringReader(""));
val result = in.peek();
