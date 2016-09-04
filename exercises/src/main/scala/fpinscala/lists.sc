object lists {
  val list = (for (i <- 0 to 10) yield i).toList
  val list2 = (for (i <- 20 to 30) yield i).toList
  val list3 = (for (i <- 40 to 50) yield i).toList

  val list4 = (for (i <- 0 to 10) yield i.toDouble).toList

  // http://stackoverflow.com/a/9160068
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => Nil
    case _ :: xs    => xs
  }

  time { tail (list) }

  def setHead[A](l: List[A], h: A): List[A] =
    h :: (tail(l))

  time { setHead(list, 99) }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n-1)

  time { drop(list, 3) }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil        => Nil
    case x :: xs    => if (f(x)) dropWhile(xs, f) else xs
  }

  time { dropWhile(list, (i:Int) => i < 5) }

  def init[A](l: List[A]): List[A] = l match {
    case Nil        => Nil
    case x :: xs    => x :: init(xs)
  }

  time { init(list) }

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(xs: List[A], ys: List[A]): List[A] = ys match {
      case Nil        => xs
      case y :: ys    => loop(y :: xs, ys)
    }

    loop(Nil, l)
  }

  time { reverse(list) }

  def init2[A](l: List[A]): List[A] =
    reverse(tail(reverse(l)))

  time { init2(list) }

  def length[A](l: List[A]): Int = l match {
    case Nil      => 0
    case _ :: xs  => 1 + length(xs)
  }

  time { length(list) }

  def length2[A](l: List[A]): Int = {
    @annotation.tailrec
    def loop(n: Int, xs: List[A]): Int = xs match {
      case Nil      => n
      case _ :: ys  => loop(n+1, ys)
    }
    loop(0, l)
  }

  time { length2(list) }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case x :: xs    => foldLeft(xs, f(z, x))(f)
  }

  time { foldLeft(list, 0)(_+_) }

  def length3[A](l: List[A]) =
    foldLeft(l, 0)((n: Int, _) => n+1)

  time { length2(list) }

  def reverse2[A](l: List[A]) =
    foldLeft(l, List[A]())((xs: List[A], x: A) => x :: xs)


  time { reverse2(list) }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B) =
    foldLeft(reverse2(l), z)((b: B, a: A) => f(a, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil      => Nil
    case x :: xs  => f(x) :: map(xs)(f)
  }

  time { map(list)((x: Int) => x * x) }

  def map2[A,B](l: List[A])(f: A => B): List[B] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B]): List[B] = as match {
      case Nil      => bs
      case a :: as_ => loop(as_, f(a) :: bs)
    }

    reverse(loop(l, Nil))
  }

  time { map2(list)((x: Int) => x * x) }

  def map3[A,B](l: List[A])(f: A => B) =
    foldRight(l, List[B]())((a: A, bs: List[B]) => f(a) :: bs)

  time { map3(list)((x: Int) => x * x) }

  import Numeric.Implicits._
  def sum[A: Numeric](l: List[A]) =
    foldLeft(l, implicitly[Numeric[A]].zero)(_+_)

  time { sum(list) }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B) =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  time { foldLeft(list, "")((b, a) => s"$a.$b") }

  time { foldLeft2(list, "")((b, a) => s"$a.$b") }


  def flatten[A](ls: List[List[A]]) =
    foldLeft(ls, Nil:List[A])((b, a) => b ++ a)

  flatten (List(list, list2, list3))

  map(list)(_+1)

  map(list4)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((a, b) => if (f(a)) a :: b else b)

  time { filter(list)(a => a % 2 == 0) }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  flatMap(List(1,2,3))(i => List(i,i))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else List())

  time { filter2(list)(a => a % 2 == 0) }

  def listAdd(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (_, Nil) | (Nil, _)      => Nil
    case (x :: xs_, y :: ys_)     => (x + y) :: listAdd(xs_, ys_)
  }

  time { listAdd(list, list2) }


  def listAdd2(xs: List[Int], ys: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(xs: List[Int], ys: List[Int], zs: List[Int]): List[Int] = (xs, ys) match {
      case (_, Nil) | (Nil, _)      => zs
      case (x :: xs_, y :: ys_)     => loop(xs_, ys_, (x + y) :: zs)
    }

    reverse(loop(xs, ys, Nil))
  }

  time { listAdd2(list, list2) }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def loop(as: List[A], bs: List[B], cs: List[C]): List[C] = (as, bs) match {
      case (_, Nil) | (Nil, _)      => cs
      case (a :: as_, b :: bs_)     => loop(as_, bs_, f(a, b) :: cs)
    }

    reverse(loop(as, bs, Nil))
  }

  def listAdd3(xs: List[Int], ys: List[Int]) =
    zipWith(xs, ys)((x, y) => x + y)

  time { listAdd3(list, list2) }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def found(xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
      case (Nil, _)                         => false
      case (x :: xs_, Nil)                  => true
      case (x :: xs_, y :: ys_) if x == y   => found(xs_, ys_)
      case (x :: xs_, y :: ys_) if x != y   => found(xs_, sub)
    }

    found(sup, sub)
  }

  time { hasSubsequence(list, List(1,2,3)) }
  time { hasSubsequence(list, List(5,6,7)) }
  time { hasSubsequence(list, List(5,6,7,9)) }
}