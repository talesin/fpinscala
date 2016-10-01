
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

  numbers.take(10).toList

  numbers.take2(10).toList

  numbers.takeWhile(_ <= 10).toList

  numbers.takeWhile2(_ <= 10).toList

  numbers.takeWhile3(_ <= 10).toList

  numbers.forAll(_ < 10)

  numbers.headOption

  numbers.headOption2

  empty.headOption

  empty.headOption2

  numbers.take(10).map(_ * 2).toList

  numbers.take(10).map2(_ * 2).toList

  numbers.take(20).filter(_ % 3 == 0).toList

  numbers.take(3).append(9).toList

  numbers.take(10).drop(7).toList

  numbers.take(3).join(numbers.take(10).drop(7)).toList

  numbers.take(3).flatMap(x => numbers.take((x+1)*10).drop(x*10+7)).toList

  Stream.constant('a').take(5).toList

  Stream.from(4).take(5).toList

  Stream.fibs(1, 1).take(10).toList

  numbers.take(10).zipWith(numbers.take(12))(_+_).toList

  numbers.take(10).zipAll(Stream.fibs(1,1).take(10)).toList

  numbers.take(5).zipAll(Stream.fibs(1,1).take(10)).toList

  numbers.take(10).zipAll(Stream.fibs(1,1).take(5)).toList

  numbers.take(10).startsWith(numbers.take(3))

  numbers.take(10).startsWith(numbers.drop(3).take(3))

  numbers.take(2).startsWith(numbers.take(3))

  numbers.take(10).tail.toList

  numbers.take(5).tails.map(_.toList).toList

  numbers.take(10).hasSubsequence(numbers.drop(3).take(4))

  numbers.take(5).hasSubsequence(numbers.drop(3).take(4))

  numbers.tail.take(3).scanRight(0)(_+_).toList

  numbers.tail.take(5).scanRight(0)(_+_).toList
}