package fpinscla.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  //3.26
  def max(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  //3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => depth(left).max(depth(right)) + 1
  }

  //3.28
  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left, f), map(right, f))
  }

  //3.29
  def fold[A, B](t: Tree[A])(f: A => B)(m: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => m(fold(left)(f)(m), fold(right)(f)(m))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maxViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(_ max _ + 1)

  def mapViaFold[A, B](t: Tree[A], f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
}
