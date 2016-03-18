package de.kaubisch.fpinscala.chapter6

import scala.annotation.tailrec

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


  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A) : Rand[A] = State.unit(a)

  def both[A, B](r1: Rand[A], r2: Rand[B]) : Rand[(A,B)] = r1.map2(r2)((_,_))

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (v,s) = rng.nextInt
    (if(v < 0) -(v+1) else v, s)
  })

  def nonNegativeEven: Rand[Int] = nonNegativeInt map (i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(i => {
      val mod = i % n
      if((i + (n - 1) - mod) >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

  def double: Rand[Double] = nonNegativeInt map (i => (if(i > 0) (i - 1) else i).toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int, Double), RNG) = randIntDouble(rng)
  def randIntDouble : Rand[(Int, Double)] = both(int, double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = randDoubleInt(rng)
  def randDoubleInt : Rand[(Double, Int)] = both(double, int)

  def double3 : Rand[(Double, Double, Double)] =
    for {
      d1 <- double
      d2 <- double
      d3 <- double
    } yield (d1, d2, d3)

  def ints(count: Int)(rng:RNG): (List[Int], RNG) = State.sequence(List.fill(count)(int))(rng)

}

case class State[S, +A](run: S => (A, S)) extends AnyVal {

  def apply(s: S) : (A, S) = run(s)

  def map[B](f: A => B) : State[S, B] = flatMap(v => State.unit(f(v)))

  def map2[B, C](state: State[S,B])(f: (A, B) => C) : State[S, C] = flatMap(a => state map (b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    val (b, s3) = f(a)(s2)
    (b, s3)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A) : State[S, A] = State(s => (a, s))

  def sequence[S, A](xs: List[State[S, A]]) : State[S, List[A]] = xs match {
    case head :: tail => head.map2(sequence(tail))( _ :: _)
    case Nil => unit(Nil)
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S] : State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(input => modify[Machine]( s => stateChanges(input, s))))
    s <- get
  } yield (s.candies, s.coins)

  private def stateChanges(input: Input, state: Machine) : Machine = (input, state) match {
    case (Turn, Machine(true,_,_)) => state
    case (Turn, Machine(false, candy, _)) => state.copy(locked=true, candies=candy - 1)
    case (Coin, Machine(false, _, _)) => state
    case (Coin, Machine(true,_,coins)) => state.copy(locked=false, coins=coins + 1)
    case (_, Machine(_, 0, _)) => state
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int);