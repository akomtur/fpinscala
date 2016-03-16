package de.kaubisch.fpinscala.chapter6

import org.scalatest.{Matchers, FlatSpec, FunSuite}

/**
  * Created by kaubisch on 16.03.16.
  */
class RNGSpec extends FlatSpec with Matchers {

  "RNG" should "return next state when calling nextInt" in {
    SimpleRNG(42).nextInt._1 shouldBe 16159453
  }

  it should "return Int.MaxValue when nextInt generates Int.MaxValue" in {
    RNG.nonNegativeInt(RNGWithValue(Int.MaxValue))._1 shouldBe Int.MaxValue
  }

  it should "return Int.MaxValue when nextInt generates Int.MinValue" in {
    RNG.nonNegativeInt(RNGWithValue(Int.MinValue))._1 shouldBe Int.MaxValue
  }

  it should "return 0.0 when nextInt will generate the value '0'" in {
    RNG.double(RNGWithValue(0))._1 shouldBe 0.0d
  }
  it should "return 1.0 when nextInt will generate Int.MaxValue" in {
    RNG.double(RNGWithValue(Int.MaxValue))._1 should be < 1.0d
  }

  it should "return correct Tuple if nextInt will return the correct values" in {
    RNG.intDouble(RNGWithValues(1:: 0 :: Nil))._1 shouldBe (1, 0.0)
  }

  it should "return correct double,int tuple if nextInt will return the correct values" in {
    RNG.doubleInt(RNGWithValues(2 :: -3 :: Nil))._1 shouldBe (1.0 / Int.MaxValue, 2)
  }

  it should "return correct double triple if nextInt will return the correct values" in {
    RNG.double3(RNGWithValues(5 :: 6 :: 7 :: Nil))._1 shouldBe (4.0 / Int.MaxValue, 5.0 / Int.MaxValue, 6.0 / Int.MaxValue)
  }

  case class RNGWithValue(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, this)
  }

  case class RNGWithValues(values: List[Int]) extends RNG {
    override def nextInt: (Int, RNG) = values match {
      case head :: tail => (head, RNGWithValues(tail))
      case head :: Nil => (head, this)
    }
  }
}