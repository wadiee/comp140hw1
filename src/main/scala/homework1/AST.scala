package homework1
import java.io._
import scala.collection.mutable.Map

/** Jam general AST type */
trait AST {
  def accept[T](v: ASTVisitor[T]): T
}

/** Visitor trait for general AST type */
trait ASTVisitor[T] {
  def forBoolConstant(b: BoolConstant): T
  def forIntConstant(i:IntConstant): T
  def forEmptyConstant(e: EmptyConstant): T
  def forVariable(v: Variable): T
  def forPrimFun(f: PrimFun): T
  def forUnOpApp(u: UnOpApp): T
  def forBinOpApp(b: BinOpApp): T
  def forApp(a: App): T
  def forMapLiteral(m: MapLiteral): T
  def forIf(i: If): T
  def forLet(l: Let): T
}

/** Jam term AST type */
sealed trait Term extends AST {
  def accept[T](v: ASTVisitor[T]): T
}

/** Jam constant type */
sealed trait Constant extends Term {
  def accept[T](v: ASTVisitor[T]): T
}

/** Jam token type */
trait Token {}

/** Jam Boolean constant class */
sealed abstract class BoolConstant(value: Boolean) extends Constant with Token {
  override def accept[T](v: ASTVisitor[T]) =  v.forBoolConstant(this)
  override def toString = value toString
}

case object True extends BoolConstant(true)
case object False extends BoolConstant(false)

/** Jam empty (list) constant class, which is a singleton */
sealed abstract class EmptyConstant extends Constant with Token {
  override def accept[T](v: ASTVisitor[T]) =  v.forEmptyConstant(this)
  override def toString = "null"
}

case object Empty extends EmptyConstant

/** Jam integer constant class */
case class IntConstant(value: Int) extends Constant with Token {
  override def accept[T](v: ASTVisitor[T]) =  v.forIntConstant(this)
  override def toString = value toString
}

/** Jam primitive function Class */
case class PrimFun(name: String) extends Term with Token {
  override def accept[T](v: ASTVisitor[T]) =  v.forPrimFun(this)
  override def toString = name
}

/** Jam variable class */
case class Variable(name: String) extends Term with Token {
  override def accept[T](v: ASTVisitor[T]) =  v.forVariable(this)
  override def toString = name
}

/** Jam operator class */
case class Op(symbol: String, isUnOp: Boolean, isBinOp: Boolean) extends Token {
  def this(s: String) = this(s, false, true)  // is BinOp only!
  override def toString = symbol
}

case class KeyWord(name: String) extends Token {
  override def toString = name
}

sealed abstract class Delimiter(value: String) extends Token {
  override def toString = value
}

case object LeftParen extends Delimiter("(")
case object RightParen extends Delimiter(")")
case object LeftBrack extends Delimiter("[")
case object RightBrack extends Delimiter("]")
case object LeftBrace extends Delimiter("{")
case object RightBrace extends Delimiter("}")
case object Comma extends Delimiter(",")
case object SemiColon extends Delimiter(";")

// AST class definitions

/** Jam unary operator application class */
case class UnOpApp(rator: Op, arg: AST) extends Term {
  override def accept[T](v: ASTVisitor[T]) = v.forUnOpApp(this)
  override def toString = rator + " " + arg
}

/** Jam binary operator application class */
case class BinOpApp(rator: Op, arg1: AST, arg2: AST) extends AST {
  override def accept[T](v: ASTVisitor[T]) = v.forBinOpApp(this)
  override def toString =  "(" + arg1 + " " + rator + " " + arg2 + ")"
}

/** Jam fun (closure) class */
case class MapLiteral(vars: Array[Variable], body: AST) extends AST {
  override def accept[T](v: ASTVisitor[T]) = v.forMapLiteral(this)
  override def toString = "map " + vars.mkString(",") + (if (vars.isEmpty) "" else " ") + "to " + body
}

/** Jam function (PrimFun or MapLiteral) application class */
case class App(rator: AST, args: Array[AST]) extends Term {
  override def accept[T](v: ASTVisitor[T]) = v.forApp(this)
  override def toString =
    rator match {
      case _: Variable | _: PrimFun =>
        rator + "(" + args.mkString(", ") + ")"
      case _ =>
        "(" +  rator + ")(" + args.mkString(", ") + ")"
    }
}

/** Jam if expression class */
case class If(test: AST, conseq: AST, alt: AST) extends AST {
  override def accept[T](v: ASTVisitor[T]) = v.forIf(this)
  override def toString = "if " + test + " then " + conseq + " else " + alt
}

/** Jam let expression class */
case class Let(defs: Array[Def], body: AST) extends AST {
  override def accept[T](v: ASTVisitor[T]) = v.forLet(this)
  override def toString =  "let " + defs.mkString(" ") + " in " + body
}

/** Jam definition class */
case class Def(lhs: Variable, rhs: AST) {
  override def toString = lhs + " := " + rhs + ";"
}

/** Parsing error class */
class ParseException(s:String) extends RuntimeException(s)

/** Jam lexer class.
  *  Given a Lexer object, the next token in that input stream being
  *  processed by the Lexer is returned by static method readToken(); it
  *  throws a ParseException (a form of RuntimeException) if it
  *  encounters a syntax error.  Calling readToken() advances the cursor
  *  in the input stream to the next token.
  *
  *  The static method peek() in the Lexer class has the same behavior as
  *  readToken() except for the fact that it does not advance the cursor.
  */
class Lexer(rdr: Reader) extends StreamTokenizer(new BufferedReader(rdr)) {

  // short names for StreamTokenizer codes
  import StreamTokenizer.{TT_WORD => WORD, TT_NUMBER => NUMBER, TT_EOF => EOF, TT_EOL => EOL}

  // These "static" fields should be moved to a companion object
  val PLUS = Op("+", true, true)
  val MINUS = Op("-", true, true)
  val TIMES = new Op("*")
  val DIVIDE = new Op("/")
  val EQUALS = new Op("=")
  val NOT_EQUALS = new Op("!=")
  val LESS_THAN = new Op("<")
  val GREATER_THAN = new Op(">")
  val LESS_THAN_EQUALS = new Op("<=")
  val GREATER_THAN_EQUALS = new Op(">=")
  val NOT = Op("~", true, false)
  val AND = new Op("&")
  val OR = new Op("|")

  /* Used to support reference cells. */
  //  val Op BANG = Op("!", true, false);
  //  val Op GETS = Op("<-");
  //  val Op REF  = Op("ref", true, false);

  /* Keywords */

  val IF     = KeyWord("if")
  val THEN   = KeyWord("then")
  val ELSE   = KeyWord("else")
  val LET    = KeyWord("let")
  //  val KeyWord LETREC = KeyWord("letrec");   // Used to support letrec extension
  val IN     = KeyWord("in")
  val MAP    = KeyWord("map")
  val TO     = KeyWord("to")
  val BIND   = KeyWord(":=")

  // wordtable for classifying words (identifiers/operators) in token stream
  val wordTable: Map[String,Token] = Map()

  // Lexer peek cannot be implemented using StreamTokenizer pushBack
  // because some Tokens are composed of two StreamTokenizer tokens

  var buffer: Token = null  // holds token for peek() operation; may be null

  /* constructors */

  /** Constructs a Lexer for the contents of the specified file */
  def this(fileName: String) { this(new FileReader(fileName)) }

  /** Constructs a Lexer for in */
  def this() { this(new InputStreamReader(System.in)) }

  /* Initializes lexer tables and the StreamTokenizer that the lexer extends */
  def initLexer() {

    // configure StreamTokenizer portion of this
    resetSyntax()
    parseNumbers()
    ordinaryChar('-')
    slashSlashComments(true)
    wordChars('0','9')
    wordChars('a','z')
    wordChars('A','Z')
    wordChars('_','_')
    wordChars('?','?')
    whitespaceChars(0,' ')

    // `+' `-' `*' `/' `~' `=' `<' `>' `&' `|' `:' `;' `,' '!'
    // `(' `)' `[' `]' are ordinary characters (self-delimiting)

    initWordTable()
    buffer = null  // buffer initially empty
  }

  /** Reads tokens until next end-of-line */
  def flush() {
    eolIsSignificant(true)
    while (nextToken() != EOL)  // eat tokens until EOL
      eolIsSignificant(false)
  }

  /** Returns the next token in the input stream without consuming it */
  def peek() = {
    if (buffer eq null) buffer = readToken()
    buffer
  }

  /** Reads the next token as defined by StreamTokenizer in the input stream  (consuming it). */
  private def getToken(): Int =
  // synonymous with nextToken() except for throwing an unchecked
  // ParseException instead of a checked IOException
    try {
      val tokenType = nextToken()
      tokenType
    } catch {
      case e:IOException =>
        throw new ParseException("IOException " + e + "thrown by nextToken()")
    }


  /** Reads the next Token in the input stream (consuming it) and returns the corresponding Token object */
  def readToken(): Token = {

    /* Use getToken() to read next token and construct a Token object representing that token.  Token representations
     * for all Token classes except IntConstant are unique; a HashMap is used to avoid duplication.   Hence, eq can
     * safely be used to compare all Tokens except IntConstants for equality */
    if (buffer != null) {
      val token = buffer
      buffer = null  // clear buffer
      token
    }
    else {
      val tokenType = getToken();
      tokenType match {
        case NUMBER =>
          val intValue = nval.asInstanceOf[Int]  // nval: Double is inherited from StreamTokenizer
          if (nval == intValue.asInstanceOf[Double]) IntConstant(intValue)
          else throw new ParseException("The number " + nval + " is not a 32 bit integer")
        case WORD =>
          val optToken = wordTable.get(sval)
          optToken match {
            case None =>
              // must be new variable name
              val newVar = Variable(sval)
              wordTable += (sval -> newVar)
              newVar
            case Some(token) => token
          }
        case EOF =>  null
        case '(' =>  LeftParen
        case ')' =>  RightParen
        case '[' =>  LeftBrack
        case ']' =>  RightBrack
        // case '{' =>  LeftBrace
        // case '}' =>  RightBrace
        case ',' =>  Comma
        case ';' =>  SemiColon

        case '+' =>  PLUS
        case '-' =>  MINUS
        case '*' =>  TIMES
        case '/' =>  DIVIDE
        case '~' =>  NOT
        case '=' =>  EQUALS
        case '<' =>
          val nextTokenType = getToken()
          if (nextTokenType == '=') LESS_THAN_EQUALS
          // else if (tokenType eq '-') GETS
          else {
            pushBack()
            LESS_THAN
          }
        case '>' =>
          val nextTokenType = getToken()
          if (nextTokenType == '=') GREATER_THAN_EQUALS
          else {
            pushBack()
            GREATER_THAN
          }
        case '!' =>
          val nextTokenType = getToken()
          if (nextTokenType == '=') NOT_EQUALS
          else throw new ParseException("!" + (nextTokenType.asInstanceOf[Char]) + " is not a legal token")
        //        else { // this alternate else clause will be used in later assignments
        //         pushBack()
        //         BANG
        //        }

        case '&' =>  AND
        case '|' =>  OR
        case ':' =>  {
          val nextTokenType = getToken()
          if (nextTokenType ==  '=') BIND
          else {
            pushBack()
            throw new ParseException("`:' is not a legalken")
          }
        }
        case _ =>  /* default */
          throw new  ParseException("`" + (tokenType.asInstanceOf[Char]) + "' is not a legal token")
      }
    }
  }

  /** Initializes the table of Strings used to recognize Tokens */
  def initWordTable() {

    // constants
    // <empty>  ::= empty
    // <bool>  ::= true | false

    wordTable.put("null", Empty)
    wordTable.put("true",  True)
    wordTable.put("false", False)

    // install opeators that are words
    //    wordTable.put("ref", new Op("ref",true,false)

    // keywords: if then else let in map to :=
    wordTable.put("if",   IF)
    wordTable.put("then", THEN)
    wordTable.put("else", ELSE)
    wordTable.put("let",  LET)
    wordTable.put("in",   IN)
    wordTable.put("map",  MAP)
    wordTable.put("to",   TO)
    wordTable.put(":=",   BIND)

    // Install primitive functions
    // <prim>  ::= number? | function? | list? | empty?
    //           | cons? | cons | first | rest | arity

    wordTable.put("number?",   PrimFun("number?"))
    wordTable.put("function?", PrimFun("function?"))
    //    wordTable.put("ref?",      PrimFun("ref?"));
    wordTable.put("list?",     PrimFun("list?"))
    wordTable.put("null?",    PrimFun("null?"))
    wordTable.put("cons?",     PrimFun("cons?"))
    wordTable.put("arity",     PrimFun("arity"))
    wordTable.put("cons",      PrimFun("cons"))
    wordTable.put("first",     PrimFun("first"))
    wordTable.put("rest",      PrimFun("rest"))
  }

  /* Body of Lexer class; intializes lexer state */
  initLexer()
}