object gettingstarted {
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(xs: Array[A], sorted: Boolean): Boolean = {
      xs match {
        case Array(a, b, _*) if sorted  => loop(xs.tail, gt(b, a))
        case _                          => sorted
      }
    }
    loop(as, true)
  }


  isSorted(Array(1,2,3,4), (a: Int, b: Int) => a > b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  val add = curry((a:Int, b:Int) => a + b)

  val add1 = add(1)

  add1(2)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  val add_ = uncurry(add)

  add_(1, 2)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  val nummer = compose((b: String) => b.reverse.toInt, (a: Int) => a.toString)

  nummer(29)
}