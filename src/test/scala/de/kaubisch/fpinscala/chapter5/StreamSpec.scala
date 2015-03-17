package de.kaubisch.fpinscala.chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  "A Stream" should "convert a Stream to List when calling toList" in {
    Stream(1,2,3,4,5,6).toList shouldBe List(1,2,3,4,5,6)
  }

  it should "return empty List when call toList on empty Stream" in {
    Stream.empty.toList shouldBe Nil
  }

  it should "return a large List when calling toList on a large Stream" in {
    Stream(1 to 5000 : _*).toList shouldBe List(1 to 5000 : _*)
  }

  it should "return first element when calling take(1)" in {
    Stream(1,2,3,4,5).take(1).toList shouldBe List(1)
  }

  it should "return first two elements when calling take(2)" in {
    Stream(1,2,3,4,5).take(2).toList shouldBe List(1,2)
  }

  it should "return only existing elements when calling take with too many elements" in {
    Stream(1,2,3).take(4).toList shouldBe List(1,2,3)
  }

  it should "return an empty Stream when calling take on empty Stream" in {
    Stream.empty.take(1) shouldBe Stream.empty
  }

  it should "return an empty Stream when calling take(0)" in {
    Stream(1,2,3).take(0) shouldBe Stream.empty
  }

  it should "return an new large stream when take a large number on a large Stream" in {
    Stream(1 to 10000 : _*).take(9999).toList shouldBe List(1 to 9999 : _*)
  }

  it should "drop the first element when calling drop(1)" in {
    Stream(1,2,3).drop(1).toList shouldBe List(2,3)
  }

  it should "drop the first two elements when calling drop(2)" in {
    Stream(1,2,3).drop(2).toList shouldBe List(3)
  }

  it should "return an empty Stream when calling drop on empty Stream" in {
    Stream.empty.drop(1) shouldBe Stream.empty
  }

  it should "return an empty Stream when dropping more elements then available" in {
    Stream(1,2,3).drop(4) shouldBe Stream.empty
  }

  it should "return an empty Stream when calling takeWhile on empty Stream" in {
    Stream.empty[Int].takeWhile(_ > 2) shouldBe Stream.empty
  }

  it should "return new filtered Stream when calling takeWhile with filter function" in {
    Stream(1,2,3).takeWhile(_ < 2).toList shouldBe List(1)
  }

  it should "return an empty Stream when calling takeWhile with function that doesn't match" in {
    Stream(1,2,3).takeWhile(_ > 5) shouldBe Stream.empty
  }
}
