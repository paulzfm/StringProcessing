/**
  * Created by paul on 26/10/2016.
  */

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestLearner extends FunSuite {

  test("Month Extraction") {
    println("[INFO] Month Extraction")
    val l = new Learner(List(
      (Vector("01/21/1998"), "01"),
      (Vector("22.12.1999"), "12"),
      (Vector("2000-23-07"), "07")
    ))

    val p = l.learn.head
    println(p.code())
    assert(p.eval(Vector("01/21/2001")).contains("01"))
    assert(p.eval(Vector("22.02.2002")).contains("02"))
    assert(p.eval(Vector("2003-23-03")).contains("03"))
  }

  test("Name Parsing") {
    println("[INFO] Name Parsing")
    val l = new Learner(List(
      (Vector("Otis", "Daniels"), "Otis, D."),
      (Vector("Kimberly", "Jones"), "Kimberly, J.")
    ))

    val p = l.learn.head
    println(p.code())
    assert(p.eval(Vector("Mary", "Leslie")).contains("Mary, L."))
  }

  test("Pick the Last Number") {
    println("[INFO] Pick the Last Number")
    val l = new Learner(List(
      (Vector("123-456-78"), "78"),
      (Vector("12-34-56-78-90"), "90")
    ))

    val p = l.learn.head
    println(p.code())
    assert(p.eval(Vector("12-12-12-123")).contains("123"))
  }
}
