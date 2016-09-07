object tree {
  sealed trait Tree[+A]
  case class Empty[A]() extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def build(n: Int): Tree[Int] = {
    val rnd = (x: Int) => util.Random.nextInt(Math.abs(n)+10)
    if (n == 0 || rnd(n) <= 10)
      Leaf(rnd(99))
    else if (rnd(n) <= 5)
      Empty()
    else
      Branch(build(rnd(n)-1), build(rnd(n)-1))
  }

  val t1 = build(100)
  val t2 = build(200)

  // http://stackoverflow.com/a/9160068
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  // Ex 3.25
  def size[_](t: Tree[_]): Int = t match {
    case Empty() => 0
    case Leaf(_) => 1
    case Branch(l: Tree[_], r: Tree[_]) => 1 + size(l) + size(r)
  }

  time { size(t1) }
  time { size(t2) }


  def size2[_](t: Tree[_]): Int = {
    @annotation.tailrec
    def loop(ts: List[Tree[_]], n: Int): Int = ts match {
      case Nil                                    => n
      case Empty() :: ts_                         => loop(ts_, n)
      case Leaf(_) :: ts_                         => loop(ts_, n + 1)
      case Branch(l: Tree[_], r: Tree[_]) :: ts_  => loop(l :: r :: ts_, n + 1)
    }

    loop(t :: Nil, 0)
  }

  time { size2(t1) }
  time { size2(t2) }

  // Ex 3.26
  def max(t: Tree[Int]): Int = t match {
    case Empty()                            => Int.MinValue
    case Leaf(x)                            => x
    case Branch(l: Tree[Int], r: Tree[Int]) => max(l).max(max(r))
  }

  time { max(t1) }
  time { max(t2) }

  def max2(t: Tree[Int]): Int = {
    @annotation.tailrec
    def loop(ts: List[Tree[Int]], n: Int): Int = ts match {
      case Nil                                        => n
      case Empty() :: ts_                             => loop(ts_, n)
      case Leaf(x) :: ts_                             => loop(ts_, n.max(x))
      case Branch(l: Tree[Int], r: Tree[Int]) :: ts_  => loop(l :: r :: ts_, n)
    }

    loop(t :: Nil, Int.MinValue)
  }

  time { max2(t1) }
  time { max2(t2) }

  // Ex 3.27
  def depth[_](t: Tree[_]): Int = t match {
    case Empty() => 0
    case Leaf(_) => 1
    case Branch(l: Tree[_], r: Tree[_]) => 1 + depth(l).max(depth(r))
  }

  time { depth(t1) }
  time { depth(t2) }

  // Ex 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Empty()                        => Empty()
    case Leaf(x)                        => Leaf(f(x))
    case Branch(l: Tree[_], r: Tree[_]) => Branch(map(l)(f), map(r)(f))
  }

  time { map(t1)(x => x+1) }
  time { map(t2)(x => x+1) }


  // Ex. 3.29
  def fold[A,B](t: Tree[A], z: B)(f: (B, A) => B)(g: (B,B) => B): B = t match {
    case Empty()              => z
    case Leaf(a)              => f(z, a)
    case Branch(l: Tree[A], r: Tree[A]) => g(fold(l, z)(f)(g), fold(r, z)(f)(g))
  }

  time { fold(t1, 0)((b, a) => a + b)((l, r) => l + r) }

  def size3(t: Tree[_]): Int =
    fold(t, 0)((b, _) => b + 1)((b1, b2) => b1 + b2 + 1)

  time { size3(t1) }
  time { size3(t2) }

  def max3(t: Tree[Int]): Int =
    fold(t, Int.MinValue)((b, a) => a.max(b))((l, r) => l.max(r))

  time { max3(t1) }
  time { max3(t2) }

  def depth2(t: Tree[_]): Int =
    fold(t, 0)((b, a) => b + 1)((l, r) => l.max(r) + 1)

  time { depth2(t1) }
  time { depth2(t2) }

  def fold2[A,B](t: Tree[A], z: B)(f: (B, A) => B)(g: (B,B) => B): B = {
    @annotation.tailrec
    def loop(ts: List[(Tree[A], Option[B])], acc: B): B = ts match {
      case Nil =>
        acc

      case (Branch(l: Tree[Int], r: Tree[Int]), None) :: ts_ =>
        loop((l, Some (acc)) :: (r, Some(acc)) :: ts_, acc)

      case (Branch(l: Tree[Int], r: Tree[Int]), Some (b)) :: ts_ =>
        loop((l, Some (acc)) :: (r, Some(acc)) :: ts_, g(b, acc))

      case (Leaf(a), _) :: ts_ =>
        loop(ts_, f(acc, a))

      case (Empty(), _) :: ts_ =>
        loop(ts_, acc)
    }

    loop((t, None) :: Nil, z)
  }

  time { fold2(t1, 0)((b, a) => a + b)((l, r) => l + r) }

  def size4(t: Tree[_]): Int =
    fold2(t, 0)((b, _) => b + 1)((b1, b2) => b1 + b2 + 1)

  time { size3(t1) }
  time { size3(t2) }

  def max4(t: Tree[Int]): Int =
    fold2(t, Int.MinValue)((b, a) => a.max(b))((l, r) => l.max(r))

  time { max3(t1) }
  time { max3(t2) }

  def depth3(t: Tree[_]): Int =
    fold2(t, 0)((b, a) => b + 1)((l, r) => l.max(r) + 1)

  time { depth2(t1) }
  time { depth2(t2) }

}