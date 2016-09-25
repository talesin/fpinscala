
import scala.{Stream => _}
import scala.collection.immutable.{Stream => _}

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object stream {

  val s = Stream(0, 1, 2, 3, 4)

  val ones: Stream[Int] = Stream.cons(1, ones)

  ones.take(2).toList

  ones.take(10).drop(3).toList

  val numbers: Stream[Int] = {
    def loop(x: Int): Stream[Int] =
      Stream.cons(x, loop(x+1))

    loop(0)
  }


  numbers.takeWhile(_ <= 10).toList

  numbers.forAll(_ < 10)

  numbers.takeWhile2(_ <= 10).toList

  numbers.headOption

  numbers.headOption2

  empty.headOption

  empty.headOption2

  numbers.take(10).map(_ * 2).toList

  numbers.take(20).filter(_ % 3 == 0).toList

  numbers.take(3).append(9).toList
}