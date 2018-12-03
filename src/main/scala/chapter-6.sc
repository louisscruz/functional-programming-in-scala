import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

//Simple(5L).nextInt

def nonNegativeInt(rng: RNG) = {
  val (i, r) = rng.nextInt
  (if (i < 0) -(i + 1) else i, r)
}

def double(rng: RNG) = {
  val (i, r) = rng.nextInt
  (i / (Int.MaxValue.toDouble + 1), r)
}

def boolean(rng: RNG) = rng.nextInt match { case (i, r) => (i % 2 == 0, r)}

def intDouble(rng: RNG) = {
  val (i, r1) = rng.nextInt
  val (d, r2) = double(r1)
  ((i, d), r2)
}

def doubleInt(rng: RNG) = {
  val (d, r1) = double(rng)
  val (i, r2) = r1.nextInt
  ((d, i), r2)
}

def double3(rng: RNG) = {
  val (d1, r1) = double(rng)
  val (d2, r2) = double(r1)
  val (d3, r3) = double(r2)
  ((d1, d2, d3), r3)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  if (count == 0) (List(), rng)
  else {
    val (x, r1) = rng.nextInt
    val (xs, r2) = ints(count - 1)(r1)
    (x :: xs, r2)
  }
}

def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
  @tailrec
  def acc(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
    if (count == 0) (xs, r) else {
      val (x, r2) = rng.nextInt
      acc(count - 1, r2, x :: xs)
    }
  }
  acc(count, rng, List())
}

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

unit(5)(Simple(5L))

def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

val _double = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

//_double(Simple(5L))

def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, r1) = ra(rng)
  val (b, r2) = rb(r1)
  (f(a, b), r2)
}

def both[A, B](ra: Rand[A], rb: Rand[B]) = map2(ra, rb)((_, _))

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

def sequenceViaFoldLeft[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft(unit(List[A]()))((acc, f) => map2(f, acc)(_ :: _))

def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

_ints(5)(Simple(5L))

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  val (a, r1) = f(rng)
  g(a)(r1)
}

flatMap(unit(5))(item => unit(item + 1))

def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i => {
  val mod = i % n
  if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
}}

def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(el => unit(f(el)))

def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map(State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)
}