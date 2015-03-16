package de.kaubisch.fpinscala.chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def getOrElse[B >: A](default: => B) : B = this match {
    case Some(v) => v
    case None => default
  }

  def flatMap[B](f: A => Option[B]) : Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]) : Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(v => if(f(v)) Some(v) else None)
}

object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for { aa <- a; bb <- b } yield f(aa,bb)

  def mean(xs: Seq[Double]) : Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]) : Option[Double] = mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(e => e)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft[Option[List[B]]](Some(Nil))((x, y) => x.flatMap(l => f(y) map(yy => l :+ yy)))

}
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]



sealed trait Either[+E,+A] {
  def map[B](f: A => B) : Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case l @ Left(_) => l
  }

  def flatMap[ EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case l @ Left(_) => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case r @ Right(_) => r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = for(a <- this; bb <- b) yield f(a,bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(e => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_ :: _))
}

object Chapter4 extends App {

  println(Option.sequence(List(Some(1),Some(2),Some(3),Some(5),Some(4))))

  println(Left("Failure").orElse(Right("Hello World")))
}
