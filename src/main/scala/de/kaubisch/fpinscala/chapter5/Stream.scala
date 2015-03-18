package de.kaubisch.fpinscala.chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def forAll(p: A => Boolean) : Boolean = foldRight(true)((a,b) => p(a) && b)

  def headOption : Option[A] =
    foldRight(None : Option[A])((a,_) => Some(a))

  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], l : List[A]) : List[A] = stream match {
      case Cons(h, t) => go(t(), l :+ h())
      case _ => l
    }
    go(this, Nil)
  }

  def take(elements: Int) : Stream[A] = this match {
    case Cons(h, t) if elements > 0 => Stream.cons(h(), t().take(elements-1))
    case _ => Empty
  }

  def drop(elements: Int) :Stream[A] = this match {
    case Empty => Stream.empty
    case c @ Cons(_,_) if elements == 0 => c
    case Cons(h, t) => t().drop(elements - 1)
  }

  def takeWhile(f: A => Boolean) : Stream[A] =
    foldRight(Stream.empty[A])((a,b) => if(f(a)) Stream.cons(a, b) else Stream.empty)

  def exists(p : A => Boolean) : Boolean = foldRight(false)((a,b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B) : B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B) : Stream[B] = foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*) : Stream [A] = {
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}
