package calculator

import calculator._
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeDelta") {
    val b = 2
    val c = 4
    val a = 3
    val d1 = Polynomial.computeDelta(Var(a), Var(b), Var(c))
    assert (d1() == b*b - 4*a*c)
  }

  test("computeSolutions") {
    var b = 2
    var c = 4
    var a = 3
    var d = b*b - 4*a*c
    var s = Polynomial.computeSolutions(Var(a), Var(b), Var(c), Var(d))
    assert (s().size == 0)

    b = 2
    c = 1
    a = 1
    d = b*b - 4*a*c
    s = Polynomial.computeSolutions(Var(a), Var(b), Var(c), Var(d))
    assert (s().size == 1)

    b = 3
    c = 2
    a = 1
    d = b*b - 4*a*c
    s = Polynomial.computeSolutions(Var(a), Var(b), Var(c), Var(d))
    assert (s().size == 2)
  }

  test("computeSolutions2") {
    val b = Var[Double](2)
    val c = Var[Double](4)
    val a = Var[Double](3)
    val delta = Polynomial.computeDelta(a, b, c)

    val s = Polynomial.computeSolutions(a, b, c, delta)
    assert (s().size == 0)

    b() = 3
    c() = 2
    a() = 1
    assert (s().size == 2)
    assert (s().head == -1)
    assert (s().last == -2)

    b() = 2
    c() = 1
    a() = 1
    assert (s().size == 1)
    assert (s().head == -1)
  }

  test("Calculator eval") {
    var ref = Map[String, Signal[Expr]]()
    var d1 = Calculator.eval(Literal(0.5), ref)
    assert (d1 == 0.5)

    val b = Plus(Literal(0.5), Literal(1.5))
    ref += ("b"->Signal(b))
    d1 = Calculator.eval(b, ref)
    assert (d1 == 2)

    val c = Minus(Literal(0.5), Literal(1.5))
    ref += ("c"->Signal(c))
    d1 = Calculator.eval(c, ref)
    assert (d1 == -1)

    d1 = Calculator.eval(Ref("c"), ref)
    assert (d1 == -1)

    val r = Times(Ref("c"), Ref("b"))
    d1 = Calculator.eval(r, ref)
    assert (d1 == -2)
  }

  test("Calculator computeValues") {
    var ref = Map[String, Var[Expr]]()
    ref += ("l"->Var(Literal(0.5)))
    ref += ("b"->Var(Plus(Literal(0.5), Literal(1.5))))
    ref += ("c"->Var(Minus(Literal(0.5), Literal(1.5))))
    ref += ("a"->Var(Ref("c")))
    ref += ("d"->Var(Times(Ref("c"), Ref("b"))))
    ref += ("f"->Var(Divide(Ref("d"), Literal(0.5))))
    var res = Calculator.computeValues(ref)
    assert (res("l")()== 0.5)
    assert (res("b")()== 2)
    assert (res("c")()== -1)
    assert (res("a")()== -1)
    assert (res("d")()== -2)
    assert (res("f")()== -4)

    ref("b")() = Literal(12)
   // res = Calculator.computeValues(ref)
    assert (res("b")()== 12)
    assert (res("d")()== -12)
    assert (res("f")()== -24)

    ref += ("n"->Var(Divide(Ref("noo"), Literal(0.5))))
    res = Calculator.computeValues(ref)
    assert (res("n")().isNaN)

    ref += ("n1"->Var(Divide(Ref("n1"), Literal(0.5))))
    res = Calculator.computeValues(ref)
    assert (res("n1")().isNaN)

    ref("b")() = Plus(Ref("d"), Literal(1.5))
    assert (res("b")().isNaN)
    assert (res("d")().isNaN)

    ref("b")() = Plus(Ref("l"), Ref("c"))
    assert (res("b")()== -0.5)
    assert (res("d")()== 0.5)
    assert (res("a")()== -1)
    assert (res("f")()== 1)
  }
}
