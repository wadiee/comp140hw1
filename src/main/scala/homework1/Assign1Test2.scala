package homework1

import junit.framework.TestCase
import java.io._
import junit.framework.Assert._

/**
 * Created by ziliangzhu on 1/25/15.
 */
class Assign1Test2 extends TestCase {

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

}
