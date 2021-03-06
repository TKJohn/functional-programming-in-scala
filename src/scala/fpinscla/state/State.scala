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

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = (rng: RNG) => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  })

  // 6.9
  def mapViaFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S, +A](run: S => (A, S)) {
  // 6.10
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = this.run(s)
      (f(a), s1)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s1) = this.run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    })
}

object State {
  // 6.10
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty))((r, acc) => r.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

// 6.11
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  // Curring update than instance method, helps chain methods with `compose`
  def update(input: Input)(machine: Machine): Machine = {
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine // A machine that’s out of candy ignores all inputs.
      case (Turn, Machine(true, _, _)) => machine // Turning the knob on a locked machine does nothing.
      case (Coin, Machine(false, _, _)) => machine // Inserting a coin into an unlocked machine does nothing.

      //Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)

      //Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
    }
  }

  import fpinscla.state.State.{get, modify, sequence}

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    // _ <- sequence(inputs.map(modify[Machine] _ compose update))
    // _ <- sequence(inputs.map((modify[Machine] _) compose update))
    _ <- sequence(inputs.map(input => modify(update(input))))
    s <- get
  } yield (s.coins, s.candies)
}
