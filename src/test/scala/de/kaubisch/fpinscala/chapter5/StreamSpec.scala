package de.kaubisch.fpinscala.chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "Stream.toList" should "convert a Stream to List" in {
    Stream(1,2,3,4,5,6).toList shouldBe List(1,2,3,4,5,6)
  }

  it should "return empty List when called on an empty Stream" in {
    Stream.empty.toList shouldBe Nil
  }

  it should "return a large List when called on a large Stream" in {
    Stream(1 to 5000 : _*).toList shouldBe List(1 to 5000 : _*)
  }

  "Stream.take" should "return first element when called with one" in {
    Stream(1,2,3,4,5).take(1).toList shouldBe List(1)
  }

  it should "return first two elements when called with two" in {
    Stream(1,2,3,4,5).take(2).toList shouldBe List(1,2)
  }

  it should "return only existing elements when called with too many elements" in {
    Stream(1,2,3).take(4).toList shouldBe List(1,2,3)
  }

  it should "return an empty Stream when called on an empty Stream" in {
    Stream.empty.take(1) shouldBe Stream.empty
  }

  it should "return an empty Stream when called with zero" in {
    Stream(1,2,3).take(0) shouldBe Stream.empty
  }

  it should "return an new large stream when take a large number on a large Stream" in {
    Stream(1 to 10000 : _*).take(9999).toList shouldBe List(1 to 9999 : _*)
  }

  "Stream.drop" should "drop the first element when calles with one" in {
    Stream(1,2,3).drop(1).toList shouldBe List(2,3)
  }

  it should "drop the first two elements when called with two" in {
    Stream(1,2,3).drop(2).toList shouldBe List(3)
  }

  it should "return an empty Stream when called on an empty Stream" in {
    Stream.empty.drop(1) shouldBe Stream.empty
  }

  it should "return an empty Stream when called with more elements then available" in {
    Stream(1,2,3).drop(4) shouldBe Stream.empty
  }

  "Stream.headOption" should "return a value when calling on filled Stream" in {
    Stream(1,2,3).headOption shouldBe Some(1)
  }

  it should "return None when calling on empty Stream" in {
    Stream.empty.headOption shouldBe None
  }

  "Stream.takeWhile" should "return an empty Stream when calling on empty Stream" in {
    Stream.empty[Int].takeWhile(_ > 2) shouldBe Stream.empty
  }

  it should "return new filtered Stream when calling with filter function" in {
    Stream(1,2,3).takeWhile(_ < 2).toList shouldBe List(1)
  }

  it should "return an empty Stream when calling with function that doesn't match" in {
    Stream(1,2,3).takeWhile(_ > 5) shouldBe Stream.empty
  }

  "Stream.forAll" should "return true when be called on an empty Stream" in {
    Stream.empty[Int].forAll(_ == 0) shouldBe true
  }

  it should "return true when all elements match" in {
    Stream(1,2,3).forAll(_ < 4) shouldBe true
  }

  it should "return false when one element doesn't match function" in {
    Stream(1,2,3).forAll(_ < 3) shouldBe false
  }

}
