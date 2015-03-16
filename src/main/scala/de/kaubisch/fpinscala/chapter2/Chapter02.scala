package de.kaubisch.fpinscala.chapter2

/**
 * Created by kaubisch on 02.03.15.
 */
object Chapter02 extends App {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    (1 to as.length - 1).forall(i  => ordered(as(i-1), as(i)))
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a:A) => (b:B) => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }

  println("isSorted:" + isSorted(Array(1,2,3,4,5,6), (a: Int, b: Int) => a < b))
  println("isSorted:" + isSorted(Array(1,2,3,4,6,5), (a: Int, b: Int) => a < b))

  val curried: (Int) => (Int) => Int = curry((a: Int, b: Int) => a + b)
  println("curry:" + curried(1)(2))
  println("uncurry:" + uncurry(curried)(2,3))

  println("compose:" + compose((b:Int) => 100+b, (a: String) => a.toInt)("20"))
}
