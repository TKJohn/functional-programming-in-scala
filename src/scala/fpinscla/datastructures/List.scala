// Ch 3

package fpinscla.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, trail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // p3.2
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, trail) => trail
    }

  // p3.3
  def setHead[A](as: List[A], h: A): List[A] =
    as match {
      case Nil => Cons(h, Nil)
      case Cons(_, trail) => Cons(h, trail)
    }

  // p3.4
  def drop[A](as: List[A], n: Int): List[A] = {

    @tailrec
    def go(as_ : List[A], n_ : Int): List[A] =
      if (n_ == 0) as_
      else go(tail(as_), n_ - 1)

    go(as, n)
  }

  // p3.5
  def dropWith[A](as: List[A], f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(head, trail) => if (f(head)) {
        dropWith(trail, f)
      } else {
        Cons(head, dropWith(trail, f))
      }
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(head, trail) => Cons(head, append(trail, a2))
    }

  //p3.6
  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, trail) => Cons(head, init(trail))
    }
}


object Main {

  import fpinscla.datastructures.List._

  def main(args: Array[String]): Unit = {

    //p3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    System.out.println(x)


    //
    System.out.println(List.init[Int](List(1, 2, 3, 4, 5, 6)))
  }
}
