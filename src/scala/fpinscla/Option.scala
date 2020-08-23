package fpinscla

//hide std library `Option` and `Either`, since we are writing our own in this chapter

import scala.math.pow
import scala.{Either => _, Option => _};


sealed trait Option[+A] {

  case class Some[+A](value: A) extends Option[A]

  case object None extends Option[Nothing]

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


  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  //4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(a => pow(a - m, 2))))
}
