package fpinscla.errorhandling

import scala.math.pow
//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _};

sealed trait Option[+A] {
  //4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap(x => if (f(x)) Some(x) else None)
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  //4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(a => pow(a - m, 2))))

  //4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(_a => b.map(_b => f(_a, _b)))

  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
      case Nil => Some(Nil)
    }

  //4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
      case Nil => Some(Nil)
    }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
