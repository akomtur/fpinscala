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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v,s) = rng.nextInt
    (if(v < 0) -(v+1) else v, s)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, s) = nonNegativeInt(rng)
    ((if(v > 0) (v - 1) else v).toDouble / Int.MaxValue, s)
  }

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
}
