package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  implicit class Tuple2Ext[A, B](val t: (A, B)) {
    def map[C](f: (A, B) => C) = f(t._1, t._2)
  }

  implicit class TupleRng[A](val t: (A, RNG)) {
    def map[B](f: RNG => (B, RNG)) = {
      val (a, r1) = t
      val (b, r2) = f(r1)
      ((a, b), r2)
    }
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map$[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    map(int)(x => if (x < 0) -(x + 1) else x)(rng)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_.toDouble.-(1) / Int.MaxValue)(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    map2(nonNegativeInt, double)((a, b) => (a, b))(rng)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    map(intDouble)(a => a.swap)(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i, r) = int(rng)
      val (xs, r2) = ints(count-1)(r)
      (i :: xs, r2)
    }
  }

  def ints$(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  def ints$$(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence$(List.fill(count)(int))(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r1 => {
    val (a, r2) = ra(r1)
    val (b, r3) = rb(r2)
    (f(a,b), r3)
  }

  def map2$[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map$(rb)(b => f(a,b)))

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => r => (Nil, r)
    case (f :: fs_) => map2(f, sequence(fs_))(_::_)
  }

  def sequence$[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]]((r:RNG) => (Nil, r))((x, xs) => map2(x, xs)(_::_))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = r1 => {
    val (a, r2) = f(r1)
    val (b, r3) = g(a)(r2)
    (b, r3)
  }


}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s1 => {
    val (a, s2) = run(s1)
    f(a).run(s2)
  })

}


object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
    State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S,A](ss: List[State[S,A]]): State[S,List[A]] =
    ss.foldRight[State[S,List[A]]](unit(Nil))((x, xs) => x.map2(xs)(_::_))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def update(m: Machine, i: Input): Machine = (m, i) match {
    case (Machine(_, 0, _), _)                        => m
    case (Machine(true, _, _), Turn)                  => m
    case (Machine(false, _, _), Coin)                 => m
    case (Machine(true, candies, coins), Coin)        => Machine(locked = false, candies, coins+1)
    case (Machine(false, candies, coins), Turn)       => Machine(locked = true, candies-1, coins)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(i => State.modify[Machine](m => update(m, i))))
    m <- State.get
  } yield (m.candies, m.coins)
}