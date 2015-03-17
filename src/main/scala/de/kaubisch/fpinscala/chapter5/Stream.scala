package de.kaubisch.fpinscala.chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption : Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], l : List[A]) : List[A] = stream match {
      case Cons(h, t) => go(t(), l :+ h())
      case _ => l
    }
    go(this, Nil)
  }

  def take(elements: Int) : Stream[A] = this match {
    case Cons(h, t) if elements > 0 => Cons(h,() => t().take(elements-1))
    case _ => Empty
  }


  def drop(elements: Int) :Stream[A] = this match {
    case Empty => Empty
    case c @ Cons(_,_) if elements == 0 => c
    case Cons(h, t) => t().drop(elements - 1)
  }

  def takeWhile(p: A => Boolean) : Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }
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
