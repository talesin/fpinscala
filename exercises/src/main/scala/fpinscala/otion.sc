object option {

  import scala.{Option => _, Some => _, Either => _, _}

  // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map (f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map (x => Some (x)) getOrElse ob

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some (a) else None)
      //if (map (f) getOrElse false) this else None
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object Option {
    def failingFn(i: Int): Int = {
      val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
      try {
        val x = 42 + 5
        x + y
      }
      catch {
        case e: Exception => 43
      } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    }

    def failingFn2(i: Int): Int = {
      try {
        val x = 42 + 5
        x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      }
      catch {
        case e: Exception => 43
      }
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean (xs) flatMap (m => mean (xs map (x => math.pow(x - m, 2))))

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap (x => b map (y => f(x, y)))

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      traverse(a)(identity)

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some (Nil))((x, acc) => map2 (f(x), acc)((x_, xs) => x_ :: xs))
  }

  None map (x => 1)

  Some ('a') map (x => x.toInt)

  None getOrElse 1

  Some ('a') getOrElse 'b'

  None flatMap (x => Some (x))

  Some ('a') flatMap (x => Some (x+1))

  None orElse Some ('a')

  Some ('x') orElse Some ('a')

  Option.mean (Seq(1.1, 2.2, 3.3, 4.4, 5.5))

  Option.variance (Seq(1.1, 2.2, 3.3, 4.4, 5.5))

  Option.map2 (Some (1), Some (2))(_+_)

  Option.map2[Int,Int,Int] (None, Some (2))(_+_)

  Option.map2[Int,Int,Int] (Some (1), None)(_+_)

  Option.sequence(List(Some(1), Some(2), Some(3)))

  Option.sequence(List(Some(1), Some(2), None, Some(3)))

  Option.traverse(List(1,2,3,4,5))(a => if (a < 6) Some (a) else None)

  Option.traverse(List(1,2,3,4,5))(a => if (a < 4) Some (a) else None)
}