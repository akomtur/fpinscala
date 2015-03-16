package de.kaubisch.fpinscala.chapter3

import scala.annotation.tailrec

/**
 * Created by kaubisch on 03.01.15.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def maximum(tree: Tree[Int]) : Int = fold(tree)((i: Int) => i)(_ max _)

  def size[A](t: Tree[A]) : Int = fold(t)(_=>1)(_+_+1)

  def depth[A](t: Tree[A]) : Int = fold(t)(_=>1)(_ max _ + 1)

  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B) : B = t match {
    case Leaf(value) => f(value)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

object TreeApp extends App {
  val tree: Tree[String] = Branch(Leaf("hello"), Branch(Leaf("a"), Leaf("b")))
  val treeNumber: Tree[Int] = Branch(Leaf(100), Branch(Leaf(2), Leaf(3)))
  println(Tree.size(tree))
  println(Tree.depth(tree))
  println(Tree.maximum(treeNumber))
}