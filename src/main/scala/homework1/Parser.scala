package homework1

import java.io._
import java.text.ParseException
import java.util._


import scala.collection.mutable.ArrayBuffer

class Parser(in: Lexer) {

  /** Convenience constructor that takes a Reader as input */

  def this(inputStream: Reader) {
    this(new Lexer(inputStream))
  }

  def this(fileName: String) {
    this(new FileReader(fileName))
  }

  def lexer(): Lexer = in

  /** Parses the program text in the lexer bound to 'in' and returns the corresponding AST. 
    * @throws ParseException if a syntax error is encountered (including lexical errors). 
    */
  def parse(): AST = in.peek() match {
    case null => throw new ParseException("EOF")
    case _ => {
      val result = parseExp(in.readToken())
      in.peek() match {
        case null => result
        case _ => throw new ParseException("Trailing garbage")
      }
    }
  }

  /** Parses:
    * <exp> :: = if <exp> then <exp> else <exp>
    * | let <prop-def-list> in <exp>
    * | map <id-list> to <exp>
    * | <term> { <biop> <exp> }
    */
  private def parseExp(token: Token): AST = token match {
    case keyWord: KeyWord => keyWord.name match {
      case "if" => {
        val ifContent = parseExp(in.readToken())
        in.readToken() match {
          case kw: KeyWord => kw.name match {
            case "then" => {
              val thenContent = parseExp(in.readToken())
              in.readToken() match {
                case kw2: KeyWord => kw2.name match {
                  case "else" => new If(ifContent, thenContent, parseExp(in.readToken()))
                  case _ => throw new ParseException("The content of Keyword is not else")
                }
                case _ => throw new ParseException("What should be Keyword else is not keyword")
              }
            }
            case _ => throw new ParseException("The content of the keyword is not then")
          }
          case _ => throw new ParseException("What should be then is not then")
        }
      }
      case "let" => {
        in.peek() match {
          case KeyWord(_) => throw new ParseException("let with no Def")
          case _ => {
            val list = parseDefList()
            in.readToken() match {
              case keyw: KeyWord => {
                keyw.name match {
                  case "in" => new Let(list, parseExp(in.readToken()))
                  case _ => throw new ParseException("let no followed by in")
                }
              }
              case _ => throw new ParseException("let no followed by keyword")
            }

          }
        }

      }
      case "map" => {
        val list = parseIdList()
        in.readToken()
        new MapLiteral(list, parseExp(in.readToken()))
      }
      case _ => throw new ParseException("Not if | Let | Map")
    }
    case _ => {
      val term = parseTerm(token)
      val s = in.peek()
      in.peek() match {
        case Op(_, _, _) => new BinOpApp(in.readToken().asInstanceOf[Op], term, parseExp(in.readToken()))
        case _ => term
      }
    }
  }

  /**
   * Factor      ::= ( Exp ) | Prim | Id
   */
  private def parseFactor(token: Token): AST = token match {
    case prim: PrimFun => prim
    case variable: Variable => variable
    case LeftParen => {
      val returnVal = parseExp(in.readToken())
      in.readToken() match {
        case RightParen => returnVal
        case _ => throw new ParseException("LeftParen not paired with RightParen")
      }
    }
    case _ => throw new ParseException("ParseFactor not matching any case.")
  }

  private def parseDef(token: Token): Def = token match {
    case variable: Variable => in.readToken() match {
      case kw: KeyWord => kw.name match {
        case ":=" => {
          val returnVal = new Def(variable, parseExp(in.readToken()))
          in.readToken() match {
            case SemiColon => returnVal
            case _ => throw new ParseException("Def not ended with Semicolon")
          }
        }
        case _ => throw new ParseException("Def keyword is not :=")
      }
      case _ => throw new ParseException("Def variable not followed by KeyWord")
    }
    case _ => throw new ParseException("Def is not started with Variable, started with " + token.getClass)
  }

  private def parseDefList(): Array[Def] = in.peek() match {
    case kw: KeyWord => {
      kw.name match {
        case "in" => Array()
        case _ => throw new ParseException("let Def+ not followed by in")
      }
    }
    case _ => Array(parseDef(in.readToken())) ++ parseDefList()
  }


  private def parseIdList(): Array[Variable] = in.peek() match {
    case Variable(_) => {
      in.readToken() match {
        case v: Variable => Array(v) ++ parseIdList()
      }
    }
    case Comma => {
      in.readToken()
      in.peek() match {
        case Variable(_) => parseIdList()
        case _ => throw new ParseException("Illegal argument after Comma")
      }
    }
    case keyword: KeyWord => keyword.name match {
      case "to" => Array[Variable]()
      case _ => throw new ParseException("the keyword after map is not 'to'")
    }
    case _ => throw new ParseException("map [illegalToken] to")
  }

  private def parseExpList(): Array[AST] = in.peek() match {
    case null => Array()
    case Comma => {
      in.readToken()
      in.peek() match {
        case RightParen => throw new ParseException("Comma is directly followed by a Right Paren")
        case _ => parseExpList()
      }
    }
    case RightParen => Array()
    case _ => {
      Array(parseExp(in.readToken())) ++ parseExpList()
    }
  }

  /** Parses:
    * <term>     ::= { <unop> } <term> | <constant> | <factor> {( <exp-list> )}
    * <constant> ::= <empty> | <int> | <bool>
    * @param token   first token in input stream to be parsed; remainder in Lexer named in.
    */
  private def parseTerm(token: Token): AST = {
    if (token.isInstanceOf[Op]) {
      val op = token.asInstanceOf[Op]
      if (!op.isUnOp) throw new ParseException("unary operator")
      else new UnOpApp(op, parseTerm(in.readToken()))
    }
    else if (token.isInstanceOf[Constant]) token.asInstanceOf[Constant]
    else {
      val factor = parseFactor(token)
      val next = in.peek()
      if (next eq LeftParen) {
        in.readToken() // remove left paren from input stream
        in.peek() match {
          case Comma => throw new ParseException("Left paren is followed directly by a comma")
          case _ => {
            val exps = parseExpList()
            in.peek() match {
              case RightParen => {
                in.readToken()
              }
              case _ => throw new ParseException("Left paren is not matched with right paren")
            }
            App(factor, exps)
          }
        }

      }
      else factor
    }
  }
}