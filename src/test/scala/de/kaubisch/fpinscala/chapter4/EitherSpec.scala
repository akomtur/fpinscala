package de.kaubisch.fpinscala.chapter4

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by kaubisch on 16.03.15.
 */
class EitherSpec extends FlatSpec with Matchers {

  "Either" should " map value to new Either when value exists" in {
    Right("123").map(_.toInt) shouldBe Right(123)
  }

  it should "not map value to new Either when value is missing" in {
    Left("123").map[Int]((a: String) => a.toInt) shouldBe Left("123")
  }

  it should "flatMap value to new Either when value exists" in {
    Right("123").flatMap(_=> Right("345")) shouldBe Right("345")
  }

  it should "not call flatMap function when value is missing" in {
    Left("error").flatMap(_ => Right("success")) shouldBe Left("error")
  }

  it should "return Either passed with orElse function when value is missing" in {
    Left("error") orElse Right("success") shouldBe Right("success")
  }

  it should "return current Either with orElse function when value exists" in {
    Right("success") orElse Left("error") shouldBe Right("success")
  }

  it should "return a new Either when calling map2 with calculated value when all values of Eithers exists" in {
    Right("abc").map2(Right("123"))(_+_) shouldBe Right("abc123")
  }

  it should "return an error state when calling map2 with passes Either" in {
    Right("abc").map2(Left("error"))(_+_) shouldBe Left("error")
  }

  it should "return an error state when calling map2 on an Either with an error" in {
    Left("error").map2(Right("success"))((a: String,b: String) => a+b) shouldBe Left("error")
  }

  it should "return an error state when calling a list with an error-prone Either in it" in {
    Either.sequence(List(Right("success"), Left("error"))) shouldBe Left("error")
  }

  it should "return a List of successful values when calling sequence with a List of Either objects" in {
    Either.sequence(List(Right("success1"), Right("success2"))) shouldBe Right(List("success1", "success2"))
  }
}
