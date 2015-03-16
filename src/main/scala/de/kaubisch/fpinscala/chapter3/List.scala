package de.kaubisch.fpinscala.chapter3

import scala.annotation.tailrec

sealed trait List[+A] {
  def tail() : List[A]
}
case object Nil extends List[Nothing] {
  override def tail(): List[Nothing] = Nil
}
case class Cons[+A](head:A, tail: List[A]) extends List[A]
object List {
  def sum(ints : List[Int]) : Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_*_)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def setHead[A](head: A, list: List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => Cons(head, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int) : List[A] = l match {
    case Nil => Nil
    case Cons(_,xs) if n == 1 => xs
    case x @ Cons(_,_) if n < 1 => x
    case Cons(x,xs) if n > 1 => drop(xs, n-1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = l match {
    case Nil => Nil
    case c @ Cons(x,_) if !f(x) => c
    case Cons(_, xs) => dropWhile(xs, f)
  }

  def init[A](l: List[A]) : List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]) : Int = foldRight(l, 0)((_, acc) => acc + 1)

  def add(l: List[Int], i: Int) : List[Int] = map(l)(_ + i)

  def doubleToString(l: List[Double]) : List[String] = map(l)(_.toString)

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((x,xs) => Cons(f(x), xs))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =concat(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean) : List[A] = foldRight(as, Nil:List[A])((x,xs) => if(f(x)) Cons(x,xs) else xs)

  def append[A](as: List[A], as2: List[A]): List[A] = foldRight(as, as2)(Cons(_,_))

  def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append)

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop(l : List[A], s: List[A]) : Boolean = (l,s) match {
      case (Nil, Nil) | (_,Nil) => true
      case (Nil, Cons(_,_)) => false
      case (Cons(x,_), Cons(y,_)) if x != y => false
      case (Cons(_,xs), Cons(_,ys)) => loop(xs, ys)
    }
    sup match {
      case Nil => loop(sup, sub)
      case l @ Cons(x,xs) if loop(l, sub) => true
      case Cons(_, xs) => hasSubsequence(xs, sub)
    }
  }

  def apply[A](as:A*) : List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}


object Chapter3 extends App {

  val list = List(1,2,3)
  val list2 = List(1.0,2.0,3.0,0.0,4.0)
  println(list)
  println(List.product(list2))
  val headList: List[Int] = List.setHead(5, list)
  println(headList)
  println(List.drop(headList, 2))
  println(List.dropWhile(headList, (i: Int) => i != 3))
  println("init:" + List.init(headList))
  println(List.add(list, 3))
  println("toString:" + List.doubleToString(list2))
  println(List.filter(list)(_ > 2))

  println(List.append(list, headList))
  println(List.concat(List(list, headList)))

  println(List.flatMap(List(1,2,3))(i=>List(i,i)))

  println("hasSubsequence:"+ List.hasSubsequence(List(1,2,3,4), List(3,4)))
  println("hasSubsequence:"+ List.hasSubsequence(List(1,2,3,4), List(3,4,5)))
  println("hasSubsequence:"+ List.hasSubsequence(List(1,2,3,4), List(4)))
  println("hasSubsequence:"+ List.hasSubsequence(List(1,2,3,4), Nil))
}