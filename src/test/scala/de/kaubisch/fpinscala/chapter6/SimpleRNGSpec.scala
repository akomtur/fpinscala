package de.kaubisch.fpinscala.chapter6

import org.scalatest.{FlatSpec, Matchers}

class SimpleRNGSpec extends FlatSpec with Matchers {

  "SimpleRNG.nextInt" should "return a positive number with a seed that generates a negative number" in {
    RNG.nonNegativeInt.run(SimpleRNG(25214903928l))._2 should be > 0
  }
}
