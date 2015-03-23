package de.kaubisch.fpinscala.chapter6

trait RNG {
  def nextInt: (RNG, Int)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (RNG, Int) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    (nextRNG, (newSeed >>> 16).toInt)
  }
}

object RNG {
  val int: State[RNG, Int] = State(_.nextInt)

  def nonNegativeInt: State[RNG, Int] = State(rng => {
    val (next, n) = rng.nextInt
    (next, if (n < 0) Math.abs(n + 1) else n)
  })

  def nonNegativeEven: State[RNG,Int] = nonNegativeInt.map(i => i - i % 2)

  def double: State[RNG, Double] = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  def intDouble: State[RNG, (Int, Double)] = nonNegativeInt.map2(double)((_, _))

  def ints(count: Int): State[RNG, List[Int]] = State.sequence(List.fill(count)(int))
}



object Chapter6 extends App {
  println(RNG.ints(10).run(new SimpleRNG(1)))
  println(RNG.int.map("bla" + _.toString).run(new SimpleRNG(1)))

  println(RNG.intDouble.run(new SimpleRNG(1)))
}