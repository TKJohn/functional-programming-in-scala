// Ch 3

package fpinscla.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


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
      case Cons(_, tail) => tail
    }

  // p3.3
  def setHead[A](as: List[A], h: A): List[A] =
    as match {
      case Nil => Cons(h, Nil)
      case Cons(_, tail) => Cons(h, tail)
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
      case Cons(head, tail) => if (f(head)) {
        dropWith(tail, f)
      } else {
        Cons(head, dropWith(tail, f))
      }
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }

  //p3.6
  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }

  //p3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, count) => count + 1)

  //3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  //3.11
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((count, _) => count + 1)

  //3.12
  def reverseLeft[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, head) => Cons(head, acc))

  //3.13
  def foldRightByLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverseLeft(as), z)((b, a) => f(a, b))

  //3.14
  def appendLeft[A](l: List[A], r: List[A]): List[A] = foldRightByLeft(l, r)(Cons(_, _))

  //3.15
  def concat[A](l: List[List[A]]): List[A] = foldRightByLeft(l, Nil: List[A])(appendLeft)

  //3.16
  def add1(l: List[Int]): List[Int] = foldRightByLeft(l, Nil: List[Int])((head, acc) => Cons(head + 1, acc))

  //3.17
  def double2String(l: List[Double]): List[String] = foldRightByLeft(l, Nil: List[String])((head, acc) => Cons(head.toString, acc))

  //3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightByLeft(as, Nil: List[B])((head, acc) => Cons(f(head), acc))

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightByLeft(as, Nil: List[A])((head, acc) => if (f(head)) Cons(head, acc) else acc)

  //3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  //3.21
  def filterByFlat[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  //3.22
  def add(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(headL, tailL), Cons(headR, tailR)) => Cons(headL + headR, add(tailL, tailR))
    }
  }

  //3.23
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(headL, tailL), Cons(headR, tailR)) => Cons(f(headL, headR), zipWith(tailL, tailR)(f))
  }

  //3.24
  @tailrec
  def startWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => (h1 == h2) && startWith(t1, t2)
    }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => Nil == sub
      case _ if startWith(sup, sub) => true
      case Cons(_, tail) => hasSubsequence(tail, sub)
    }
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
    System.out.println(hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4, 5)))

    //p3.8
    val y = foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _))
    System.out.println(y)
  }
}
