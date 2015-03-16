package de.kaubisch.fpinscala.chapter1

import scala.annotation.tailrec

/**
 * Created by kaubisch on 01.01.15.
 */
object Fibonacci extends App {

  def fib(n: Int) : Int = {
    @tailrec
    def loop(sum1: Int, sum2: Int, cur: Int) : Int = n match {
      case 0 => 0
      case m if m <= cur => sum1 + sum2
      case _ => loop(sum2, sum1+sum2, cur+1)
    }
    loop(0, 1, 2)
  }

  assert( fib(0) == 0)
  println(fib(6))
}
