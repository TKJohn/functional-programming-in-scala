package fpinscla.laziness

import fpinscla.laziness.Stream.{cons, empty, unfold}

import scala.annotation.tailrec
import scala.{Stream => _}

sealed trait Stream[+A] {
  // 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList_2: List[A] = {
    @tailrec
    def run(as: Stream[A], acc: List[A]): List[A] = as match {
      case Empty => acc
      case Cons(h, t) => run(t(), h() :: acc)
    }

    run(this, List.empty).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  // 5.6
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWithViaUnfold[B, C](r: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, r) {
      case (Cons(hl, tl), Cons(hr, tr)) => Some(f(hl(), hr()), (tl(), tr()))
      case _ => None
    }

  def zipAllViaUnfold[B](r: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, r) {
      case (Cons(hl, tl), Cons(hr, tr)) => Some((Some(hl()), Some(hr())), (tl(), tr()))
      case (Empty, Cons(hr, tr)) => Some((None, Some(hr())), (empty, tr()))
      case (Cons(hl, tl), Empty) => Some((Some(hl()), None), (tl(), empty))
      case (Empty, Empty) => None
    }

  // 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAllViaUnfold(s).takeWhile(_._2.isDefined).forAll {
      case (l, r) => l equals r
    }

  // 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    } append Stream(empty)

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

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fibs(): Stream[Int] = {
    def run(n1: Int, n2: Int): Stream[Int] = cons(n1, run(n2, n1 + n2))

    run(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, nz)) => cons(a, unfold(nz)(f))
    case None => empty
  }

  // 5.12
  def fibsViaUnfold(): Stream[Int] = unfold(0, 1) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

  def onesViaUnfold(): Stream[Int] = unfold(1)(a => Some((a, a)))
}


