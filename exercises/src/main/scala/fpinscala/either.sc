object either {

  import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = {
      case Left(e)  => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      case Left(_)  => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (Right(a), Right(b_))  => Right(f(a, b_))
      case (Left(e), _)           => Left(e)
      case (_, Left(e))           => Left(e)
    }
  }
  case class Left[+E](get: E) extends Either[E,Nothing]
  case class Right[+A](get: A) extends Either[Nothing,A]

  object Either {
    def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sys.error("todo")

    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = sys.error("todo")

    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }
  }


  Left("lefty") map ((s:String) => s.toUpperCase)

  Right("righty") map (s => s.toUpperCase)

  Left("lefty") flatMap ((s:String) => Right("new right"))
  Right("righty") flatMap ((s:String) => Right("new right"))
}