package fpinscla.state

import scala.annotation.tailrec

trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, r) = rng.nextInt
    val nv = if (v < 0) -(v + 1) else v
    (nv, r)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    val o = v / Int.MaxValue
    (o, r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(rng)
    val (d3, r3) = double(rng)

    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def run(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0) return (xs, rng)

      val (v, r) = rng.nextInt

      run(count - 1, r, v :: xs)
    }

    run(count, rng, List.empty)
  }

  // 6.5
  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue)

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (va, rng2) = ra(rng)
      val (vb, rng3) = rb(rng2)
      (f(va, vb), rng3)
    }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
}
