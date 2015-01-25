package homework1

import junit.framework.Assert._
import junit.framework._
import java.io._;

class Assign1Test extends TestCase {

  protected def checkString(name: String, answer: String, program: String) {
    val p = new Parser(new StringReader(program));
    val result = p.parse()
    //   System.err.println("checkString parsed '" + result + "'")
    assertEquals(name, answer, result.toString());
  }

  protected def checkStringBad(name: String, program: String) {
    val p = new Parser(new StringReader(program));
    val result = p.parse()
  }

  protected def checkFile(name: String, answerFilename: String, programFilename: String) {
    try {
      val answerFile = new File(answerFilename)
      val fin = new BufferedInputStream(new FileInputStream(answerFile))

      val size = answerFile.length().asInstanceOf[Int];
      val data = new Array[Byte](size);
      //     System.err.println("Reading answer file " + answerFilename);
      fin.read(data, 0, size);

      val answer = new String(data);

      val p = new Parser(programFilename);
      //     System.err.println("Reading input file " + programFilename);
      assertEquals(name, answer, p.parse().toString());
    } catch {
      case e:IOException => fail("Critical error: IOException caught while reading input file");
      /* e.printStackTrace(); */
    }
  }

  def testAdd() {
    try {
      val output = "(2 + 3)";
      val input = "2+3";
      checkString("add", output, input );

    } catch {
      case e:Exception => fail("add threw " + e)
    }
  }


  def testPrim  () {
    try {
      val output = "first";
      val input = "first";
      checkString("prim  ", output, input );

    } catch {
      case e:Exception => fail("prim   threw " + e);
    }
  }

  def testSimpleMap() {
    try {
      val output = "map x to 1";
      val input = "map x to 1";
      checkString("simple map", output, input);
    }
    catch {
      case p: ParseException => fail("simpleMap threw " + p)
      case e: Exception      => fail("simpleMap threw " + e)
    }
  }

  def testParseException() {
    try {
      val output = "doh!";
      val input = "map a, to 3";
      checkString("parseException", output, input );

      fail("parseException did not throw ParseException exception");
    }
    catch {
      case p: ParseException => { /* p.printStackTrace(); */  }
      case e: Exception      => fail("parseException threw " + e)
    }
  }

  def testLet() {
    try {
      val output = "let a := 3; in (a + a)";
      val input = "let a:=3; in a + a";
      checkString("let", output, input );

    } catch {
      case e:Exception => fail("let threw " + e)
    }
  }


  def testMap() {
    try {
      val output = "map f to (map x to f(x(x)))(map x to f(x(x)))";
      val input = "map f to (map x to f( x( x ) ) ) (map x to f(x(x)))";
      checkString("map", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testMap2() {
    try {
      val output = "map x to f(x(x))";
      val input = "map x to f( x( x ) )";
      checkString("map", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testTerm() {
    try {
      val output = "f(x(x))";
      val input = "f( x( x ) )";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testTerm2() {
    try {
      val output = "x";
      val input = "( x )";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testLet2() = {
    try {
      val output = "let f := map n to if (n = 0) then 1 else (n * f((n - 1))); in f(3)";
      val input = "let f :=  map n to if n = 0 then 1 else n * f(n - 1);\nin f(3)";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testLet3() = {
    try {
      val output = "let Y := map f to let g := map x to f(map z to (x(x))(z)); in g(g); FACT := map f to map n to if (n = 0) then 1 else (n * f((n - 1))); in (Y(FACT))(3)";
      val input = "let Y    := map f to \n              let g := map x to f(map z to (x(x))(z));\n\t    in g(g);\n    FACT := map f to \n\t      map n to if n = 0 then 1 else n * f(n - 1);\nin (Y(FACT))(3)";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testLet4() = {
    try {
      val output = "let Y := map f to let g := map x to f(map z1,z2 to (x(x))(z1, z2)); in g(g); APPEND := map ap to map x,y to if (x = null) then y else cons(first(x), ap(rest(x), y)); l := cons(1, cons(2, cons(3, null))); in (Y(APPEND))(l, l)";
      val input = "let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testFactor() = {
    try {
      val output = "function?()";
      val input = "function? ()";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testExp() = {
    try {
      val output = "(1 + (2 * 3))";
      val input = "1 + 2 * 3";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testExp2() = {
    try {
      val output = "((1 + 2) * 3)";
      val input = "(1 + 2) * 3";
      checkString("", output, input );

    } catch {
      case e:Exception => fail("map threw " + e)
    }
  }

  def testMediumFactor() = {
    try {
      val output = "f()";
      val input = "f()";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMediumFactor1() = {
    try {
      val output = "f(x)";
      val input = "f(x)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMediumFacotor2() = {
    try {
      val output = "f(x, y, z)";
      val input = "f(x,y,z)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }
  def testMedium1() = {
    try {
      val output = "+ - ~ + - ~ ~ ~ false";
      val input = "+-~+-~~~false";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }
  def testMediumMap() = {
    try {
      val output = "map to (x + (y - z))";
      val input = "map to x + y - z";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }
  def testMediumLet() = {
    try {
      val output = "let f := (4 = 6); g := (12 * h(j)); h := 50; in x";
      val input = "let f := 4 = 6; g := 12 * h(j); h := 50; in x";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMedium2() = {
    try {
      val output = "(42 - - - - - - - + + + + + + + - - - + + + - + 12)";
      val input = "42-------+++++++---+++-+12";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMediumLet2() = {
    try {
      val output = "let x := 2; in let y := - x; in (map x to (x * y))(100)";
      val input = "let x := 2; in\n   let y := - x; in\n        (map x to x * y)(100)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMedium3() = {
    try {
      val output = "n";
      val input = "(n)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMediumLet3() = {
    try {
      val output = "let x := map x to f(true); in 1";
      val input = "let\n  x := map x to f(true);\nin\n  1";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMediumLet4() = {
    try {
      val output = "let x := 3; y := g(); z := map to 1; in (true)(false)";
      val input = "let \n  x:=3;\n  y:=g();\n  z:=map to 1;\nin\n  (true)(false)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testMediumLet5() = {
    try {
      val output = "let x := 3; y := 4; z := cons?(function?((x * ~ y)), cons(- arity(x))); in rest(null?(true), list?(false), first(null))";
      val input = "let x:=3;\n    y:=4;\n    z:=cons?(function?(x * ~y), cons(-arity(x))); \nin\n    rest(null?(true),list?(false),first(null))";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testHardMap() = {
    try {
      val output = "map x to let x := 3; y := 4; z := x; in (3 + map x to x)";
      val input = "map x to let x := 3; y := 4;z := x; in 3 + map x to x";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testHardMap1() = {
    try {
      val output = "map f to (map x to f(x(x)))(map x to f(x(x)))";
      val input = "map f to (map x to f( x(x))) (map x to f(x(x)))";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testHard() = {
    try {
      val output = "f(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)";
      val input = "f(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testHard1() = {
    try {
      val output = "let f := (4 = 6); in g(4, 5)";
      val input = "let f := 4 = 6; in g(4,5)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testHardLet() = {
    try {
      val output = "let mapStream := map f,l to if (l = null) then null else cons(f(first(l)), mapStream(f, rest(l))); oddNums := cons(3, mapStream(map i to (i + 2), oddNums)); filter := map p,l to if (l = null) then null else if p(first(l)) then filter(p, rest(l)) else cons(first(l), filter(p, rest(l))); divides := map a,b to (((b / a) * a) = b); initSeg := map l,n to if (n <= 0) then null else cons(first(l), initSeg(rest(l), (n - 1))); primes := map l to let p := first(l); in let l1 := filter(map x to divides(p, x), rest(l)); in cons(p, primes(l1)); in initSeg(cons(2, primes(oddNums)), 10)"
      val input = "let mapStream := map f,l to\n                   if l = null then null\n                   else cons(f(first(l)),mapStream(f,rest(l)));\n    oddNums := cons(3,mapStream(map i to i+2, oddNums));\n    filter := map p,l to\n                if l = null then null\n                else if p(first(l)) then filter(p,rest(l))\n                else cons(first(l),filter(p,rest(l)));\n     divides := map a,b to ((b/a)*a) = b;\n     initSeg := map l,n to\n                  if n <= 0 then null\n                  else cons(first(l),initSeg(rest(l),n-1));\n     primes := map l to  // l must have form cons(p,l') where p is prime,\n                         // l' contains all primes > p, but no numbers divisible\n                         //                         // by primes < p\n                         //\n                 let p := first(l);\n                 in let l1 := filter(map x to divides(p,x),rest(l));\n                    in cons(p,primes(l1));\n\nin initSeg(cons(2,primes(oddNums)),10)";
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testHardLet1() = {
    try {
      val output = "let f := map n to if (n = 0) then 1 else (n * f((n - 1))); in let f := map n,m,k to if ((n <= (0 & (n >= 0))) | (n < (0 & (n > (0 & (n != 0)))))) then number? else (m / f((k + 1))); in let x := 3; y := 4; z := cons?(function?((x * ~ y)), cons(- arity(x))); in let x := 3; y := 4; z := g(); in (g(x, y, z))(null?(true), list?(false), first(null))"
      val input = "let\n  f := map n to if n = 0 then 1 else n * f(n - 1); \nin\n  let\n    f := map n,m,k to if (n <= 0 & n >= 0)\n                  | (n < 0 & n > 0 & n != 0) then number?\n                                           else m / f(k + 1);\n  in\n     let x:=3;\n         y:=4;\n         z:=cons?(function?(x * ~y), cons(-arity(x)));\n     in\n        let x:=3;\n            y:=4;\n            z:=g();\n        in\n            (g(x,y,z))(null?(true),list?(false),first(null))"
      checkString("", output, input );

    } catch {
      case e: Exception => fail("map threw " + e)
    }
  }

  def testBad() = {
    try {
      val input = "null + 3 -";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testBad1() = {
    try {
      val input = "if number? then true";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testBad2() = {
    try {
      val input = "1 * 2 -";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testBad3() = {
    try {
      val input = "* 3 + 4";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testBad4() = {
    try {
      val input = "5;";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())

    }
  }

  def testBad5() = {
    try {
      val input = "f(a,b,^);";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testBad6() = {
    try {
      val input = "map a,b,c 5";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testBad7() = {
    try {
      val input = "-";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testBad8() = {
    try {
      val input = "8*8;";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testBad9() = {
    try {
      val input = "f[5]";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testMediumBad() = {
    try {
      val input = "f(,x)";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testMediumBad1() = {
    try {
      val input = "f(x,)";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testMediumBad2() = {
    try {
      val input = "if let x:= 3 in let y := 3 in 4 then true else false";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testMediumBad4() = {
    try {
      val input = "let to 1 + 2 - 3";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testMediumBad5() = {
    try {
      val input = "f g";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testMediumBad6() = {
    try {
      val input = "let ; in 3 = 4 + 6";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testMediumBad7() = {
    try {
      val input = "let \n  null?:=3;\nin\n  f(null?)";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testMediumBad8() = {
    try {
      val input = "let \n  null?=3;\nin\n  f(null?)";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testMediumBad9() = {
    try {
      val input = "let\n  f:= map x to f((x + y);\nin\n  if x then y else z";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }

  def testMediumBad10() = {
    try {
      val input = "let\n  f := map n to if n = 0 then 1; \nin\n  f(5)";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case _ => fail("Got other exception")
    }
  }


  def testBad11() = {
    try {
      val input = "let f := 4 = 6; g := 12 * h(j); h := 50 in x";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }

  def testBad12() = {
    try {
      val input = "let\n  f := map n to if n = 0 then 1 else n * f(n - 1); \nin\n  let\n    f := map n,m,k to if (n <= 0 & n >= 0)\n                  | (n < 0 & n > 0 & n != 0) then number?\n                                           else m / f(k + 1);\n  in\n     let x:=3;\n         y:=4;\n         z:=cons?(function?(x ^ ~y), cons(-arity(x)));\n     in\n        let x=3;\n            y:=4;\n            z:=g();\n        in\n            (g(x,y,z))(null?(true),list?(false),first(null))";
      checkStringBad("", input );
      fail("Expected ParseException")
    } catch {
      case e: ParseException =>
      case ee: Exception => fail("Got other exception" + ee.printStackTrace())
    }
  }


}