package fintech.homework04
import fintech.homework06.ComplexNumber
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.Eq._

class EqSpec extends FlatSpec with Matchers {

  it should "work correct with Eq for Option" in {
    Option(1) ==== Option(2) should be (false)
    Option(1) ==== Option(1) should be (true)
  }

  it should "work correct with Eq for Option of different types" in {
    Option(1.2) ==== Option(1.2) should be (true)
    Option(List(1,2)) ==== Option(List(1,2)) should be (true)
    Option("abc") ==== Option("def") should be (false)
  }

  it should "work correct with Eq for Map" in {
    Map(1 -> 2) ==== Map(3 -> 4) should be (false)
    Map(5 -> 6) ==== Map(5 -> 6) should be (true)
  }

  it should "work correct with Eq for ComplexNumbers" in {
    new ComplexNumber(1, 1) ==== new ComplexNumber(1, 2) should be (false)
    new ComplexNumber(1.2, 3) ==== new ComplexNumber(1.2, 3) should be (true)
  }

  it should "work correct with Eq for ComplexNumbers with scale" in {
    new ComplexNumber(1.2, 3)~2 ==== new ComplexNumber(-7.56, 7.2) should be (true)
    new ComplexNumber(1.2, 3)~2 ==== new ComplexNumber(-7.6, 7.2) should be (false)
    new ComplexNumber(1.2, 3)~2 ==== (new ComplexNumber(-7.6, 7.2), 1) should be (true)
  }
}