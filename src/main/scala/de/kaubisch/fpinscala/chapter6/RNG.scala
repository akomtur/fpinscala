package de.kaubisch.fpinscala.chapter6

/**
  * Created by kaubisch on 16.03.16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A) : Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, nextState) = s(rng)
    (f(a), nextState)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v,s) = rng.nextInt
    (if(v < 0) -(v+1) else v, s)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) = map(nonNegativeInt)(i => (if(i > 0) (i - 1) else i).toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intValue, s1) = nonNegativeInt(rng)
    val (doubleValue, s2) = double(rng)
    ((intValue, doubleValue), s2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (doubleValue, s1) = double(rng)
    val (intValue, s2)    = nonNegativeInt(s1)
    ((doubleValue, intValue), s2)
  }
  def double3(rng: RNG) : ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng:RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil, rng)
      case number: Int => {
        val (v, s) = rng.nextInt
        val (list, state) = ints(count - 1)(s)
        (v :: list, state)
      }
    }
  }
}
